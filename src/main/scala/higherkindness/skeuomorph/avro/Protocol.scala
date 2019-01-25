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

package higherkindness.skeuomorph.avro

import higherkindness.skeuomorph.mu
import higherkindness.skeuomorph.mu.{MuF, SerializationType}
import higherkindness.skeuomorph.uast.derivation._
import io.circe.Json
import org.apache.avro.{Schema, Protocol => AvroProtocol}
import qq.droste._

import scala.collection.JavaConverters._

final case class Protocol[A](
    name: String,
    namespace: Option[String],
    types: List[A],
    messages: List[Protocol.Message[A]]
)

object Protocol {

  final case class Message[A](name: String, request: A, response: A)

  object Message {
    def toJson[T](message: Message[T])(implicit T: Project[Type, T]): Json = {
      val avroToJson = scheme.cata(Type.toJson)

      Json.obj(
        "request" -> Json.arr(
          Json.obj(
            "name" -> Json.fromString("arg"), //TODO: is this doable?
            "type" -> avroToJson(message.request)
          )
        ),
        "response" -> avroToJson(message.response)
      )
    }
  }

  def fromProto[T](proto: AvroProtocol)(implicit T: Embed[Type, T]): Protocol[T] = {
    val toAvroF: Schema => T = scheme.ana(Type.fromAvro)
    def toMessage(kv: (String, AvroProtocol#Message)): Message[T] =
      Message[T](kv._2.getName, toAvroF(kv._2.getRequest), toAvroF(kv._2.getResponse))

    Protocol(
      proto.getName,
      Option(proto.getNamespace),
      proto.getTypes.asScala.toList.map(toAvroF),
      proto.getMessages.asScala.toList.map(toMessage)
    )
  }

  def toJson[T](proto: Protocol[T])(implicit T: Basis[Type, T]): Json = {
    val withNamespace: Json = Json.fromFields(proto.namespace.map("namespace" -> Json.fromString(_)).toList)

    withNamespace deepMerge Json.obj(
      "protocol" -> Json.fromString(proto.name),
      "types"    -> Json.fromValues(proto.types.map(scheme.cata(Type.toJson))),
      "messages" -> Json.fromFields(
        proto.messages.map(m => m.name -> Message.toJson(m))
      )
    )
  }

  def fromFreesFSchema[T](implicit T: Basis[Type, T]): Trans[MuF, Type, T] = ???

//  def fromFreesFSchema[T](implicit T: Basis[Type, T]): Trans[MuF, Type, T] = Trans {
//    case MuF.TNull()                => AvroF.`null`()
//    case MuF.TDouble()              => AvroF.double()
//    case MuF.TFloat()               => AvroF.float()
//    case MuF.TInt()                 => AvroF.int()
//    case MuF.TLong()                => AvroF.long()
//    case MuF.TBoolean()             => AvroF.boolean()
//    case MuF.TString()              => AvroF.string()
//    case MuF.TByteArray()           => AvroF.bytes()
//    case MuF.TNamedType(name)       => AvroF.namedType(name)
//    case MuF.TOption(value)         => AvroF.union(NonEmptyList(AvroF.`null`[T]().embed, List(value)))
//    case MuF.TEither(left, right)   => AvroF.union(NonEmptyList(left, List(right)))
//    case MuF.TList(value)           => AvroF.array(value)
//    case MuF.TMap(value)            => AvroF.map(value)
//    case MuF.TGeneric(_, _)         => ??? // WAT
//    case MuF.TRequired(t)           => T.coalgebra(t)
//    case MuF.TCoproduct(invariants) => AvroF.union(invariants)
//    case MuF.TSum(name, fields)     => AvroF.enum(name, none[String], Nil, none[String], fields)
//    case MuF.TProduct(name, fields) =>
//      TRecord(
//        name,
//        none[String],
//        Nil,
//        none[String],
//        fields.map(f => Field(f.name, Nil, none[String], none[Order], f.tpe)))
//  }

  def fromFreesFProtocol[T, U](proto: mu.Protocol[T])(implicit T: Basis[MuF, T], U: Basis[Type, U]): Protocol[U] = {
    def fromFreestyle: T => U = scheme.cata(fromFreesFSchema.algebra)
    val services: List[Message[U]] = proto.services
      .filter(_.serializationType == SerializationType.Avro)
      .flatMap { s =>
        s.operations.map(op => Message(op.name, fromFreestyle(op.request), fromFreestyle(op.response)))
      }

    Protocol(
      proto.name,
      proto.pkg,
      proto.declarations.map(fromFreestyle),
      services
    )
  }
}
