/*
 * Copyright 2018-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package higherkindness.skeuomorph.protobuf
import com.google.protobuf.DescriptorProtos.{FileDescriptorProto, MethodDescriptorProto, ServiceDescriptorProto}
import cats.data.NonEmptyList
import higherkindness.skeuomorph.mu
import higherkindness.skeuomorph.mu.{MuF, SerializationType}
import io.circe.Json
import qq.droste._
import qq.droste.syntax.all._

final case class Protocol[T](
    name: String,
    pkg: String,
    options: List[(String, String)],
    declarations: List[T],
    services: List[Protocol.Service[T]]
)

object Protocol {

  import ProtobufF._
  import NativeDescriptor._

  final case class Service[T](
      name: String,
      operations: List[Operation[T]]
  )

  final case class Operation[T](
      name: String,
      request: T,
      response: T
  )

  def fromProto[T](proto: List[FileDescriptorProto])(implicit T: Embed[ProtobufF, T]): List[Protocol[T]] = {
    val toProtobufF: NativeDescriptor => T = scheme.ana(fromProtobuf)

    def toService(s: ServiceDescriptorProto): Service[T] =
      Service[T](s.getName, s.getMethodList.j2s.map(toOperation))

    def toOperation(o: MethodDescriptorProto): Operation[T] =
      Operation[T](
        name = o.getName,
        request = toProtobufF(
          findMessage(o.getInputType, proto)
            .map(msg => toNativeMessage(msg, proto))
            .getOrElse(NativeNull())),
        response = toProtobufF(
          findMessage(o.getOutputType, proto)
            .map(msg => toNativeMessage(msg, proto))
            .getOrElse(NativeNull()))
      )

    def toProtocol(p: FileDescriptorProto): Protocol[T] =
      Protocol[T](
        name = p.getName,
        pkg = p.getPackage,
        options = Nil,
        declarations = p.getMessageTypeList.j2s.map(m => toProtobufF(toNativeMessage(m, proto))),
        services = p.getServiceList.j2s.map(s => toService(s))
      )

    proto.map(toProtocol)
  }

  def toJson[T](proto: Protocol[T])(implicit T: Project[ProtobufF, T]): Json = {
    val protobufToJson = scheme.cata(ProtobufF.toJson)
    Json.obj(
      "name"         -> Json.fromString(proto.name),
      "pkg"          -> Json.fromString(proto.pkg),
      "options"      -> Json.arr(proto.options.map(o => Json.obj(o._1 -> Json.fromString(o._2))): _*),
      "declarations" -> Json.arr(proto.declarations.map(scheme.cata(ProtobufF.toJson)): _*),
      "services" -> Json.arr(
        proto.services.map(s =>
          Json.obj(
            "name" -> Json.fromString(s.name),
            "operations" -> Json.arr(
              s.operations.map(o =>
                Json.obj(
                  "name"     -> Json.fromString(o.name),
                  "request"  -> protobufToJson(o.request),
                  "response" -> protobufToJson(o.response)
              )): _*)
        )): _*)
    )
  }

  def fromFreesFSchema[T](implicit T: Basis[ProtobufF, T]): Trans[MuF, ProtobufF, T] = Trans {
    case MuF.TNull()          => TNull()
    case MuF.TDouble()        => TDouble()
    case MuF.TFloat()         => TFloat()
    case MuF.TInt()           => TInt32()
    case MuF.TLong()          => TInt64()
    case MuF.TBoolean()       => TBool()
    case MuF.TString()        => TString()
    case MuF.TByteArray()     => TBytes()
    case MuF.TNamedType(name) => TNamedType(name)
    case MuF.TOption(value) =>
      TOneOf(
        name = "Option",
        fields = NonEmptyList(
          head = FieldF.Field[T](
            name = "",
            tpe = value,
            position = 1,
            options = Nil,
            isRepeated = false,
            isMapField = false
          ),
          tail = List(
            FieldF.Field[T](
              name = "null",
              tpe = ProtobufF.`null`[T]().embed,
              position = 2,
              options = Nil,
              isRepeated = false,
              isMapField = false))
        )
      )
    case MuF.TEither(left, right) =>
      TOneOf(
        name = "Either",
        NonEmptyList(
          head = FieldF
            .Field[T](name = "left", tpe = left, position = 1, options = Nil, isRepeated = false, isMapField = false),
          tail = List(FieldF
            .Field[T](name = "right", tpe = right, position = 2, options = Nil, isRepeated = false, isMapField = false))
        )
      )
    case MuF.TList(value)     => TRepeated(value)
    case MuF.TMap(key, value) => TMap(key.getOrElse(ProtobufF.`null`[T]().embed), value)
    case MuF.TGeneric(_, _)   => ??? // WAT
    case MuF.TContaining(v)   => TFileDescriptor(v, "", "")
    case MuF.TRequired(t)     => T.coalgebra(t)
    case MuF.TCoproduct(invariants) =>
      TOneOf(
        name = "Coproduct",
        fields = invariants.zipWithIndex.map(
          i =>
            FieldF.Field[T](
              name = "",
              tpe = i._1,
              position = i._2 + 1,
              options = Nil,
              isRepeated = false,
              isMapField = false))
      )
    case MuF.TSum(name, fields) => TEnum(name, fields.zipWithIndex, Nil, Nil)
    case MuF.TProduct(name, fields) =>
      TMessage(
        name,
        fields.zipWithIndex.map { f =>
          val (field, index) = f
          FieldF.Field[T](
            name = field.name,
            tpe = field.tpe,
            position = index + 1,
            options = Nil,
            isRepeated = false,
            isMapField = false)
        },
        Nil
      )
  }

  def fromFreesFProtocol[T, U](
      protocol: mu.Protocol[T])(implicit T: Basis[MuF, T], U: Basis[ProtobufF, U]): Protocol[U] = {
    import higherkindness.skeuomorph.mu.Service.{Operation => MuOperation}

    def fromFreestyle: T => U = scheme.cata(fromFreesFSchema.algebra)

    def fromOperation(o: MuOperation[T]): Operation[U] = Operation[U](
      name = o.name,
      request = fromFreestyle(o.request),
      response = fromFreestyle(o.response),
    )

    new Protocol(
      name = protocol.name,
      pkg = protocol.pkg.getOrElse(""),
      options = protocol.options,
      declarations = protocol.declarations.map(fromFreestyle),
      services = protocol.services
        .filter(_.serializationType == SerializationType.Protobuf)
        .map(s => Service[U](s.name, s.operations.map(fromOperation)))
    )
  }
}
