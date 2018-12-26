/*
 * Copyright 2018 47 Degrees, LLC. <http://www.47deg.com>
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

import cats.data.NonEmptyList
import cats.syntax.option._
import higherkindness.skeuomorph.mu
import higherkindness.skeuomorph.mu.{MuF, SerializationType}
import io.circe.Json
import org.apache.avro.{Schema, Protocol => AvroProtocol}
import qq.droste._
import qq.droste.syntax.all._

import scala.collection.JavaConverters._

final case class Protocol[A](
    name: String,
    namespace: Option[String],
    types: List[A],
    messages: List[Protocol.Message[A]]
)

object Protocol {
  import AvroF._

  final case class Message[A](name: String, request: A, response: A)

  object Message {
    def toJson[T](message: Message[T])(implicit T: Project[AvroF, T]): Json = {
      val avroToJson = scheme.cata(AvroF.toJson)

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

  def fromProto[T](proto: AvroProtocol)(implicit T: Embed[AvroF, T]): Protocol[T] = {
    val toAvroF: Schema => T = scheme.ana(fromAvro)
    def toMessage(kv: (String, AvroProtocol#Message)): Message[T] =
      Message[T](kv._2.getName, toAvroF(kv._2.getRequest), toAvroF(kv._2.getResponse))

    Protocol(
      proto.getName,
      Option(proto.getNamespace),
      proto.getTypes.asScala.toList.map(toAvroF),
      proto.getMessages.asScala.toList.map(toMessage)
    )
  }

  def toJson[T](proto: Protocol[T])(implicit T: Basis[AvroF, T]): Json = {
    val withNamespace: Json = Json.fromFields(proto.namespace.map("namespace" -> Json.fromString(_)).toList)

    withNamespace deepMerge Json.obj(
      "protocol" -> Json.fromString(proto.name),
      "types"    -> Json.fromValues(proto.types.map(scheme.cata(AvroF.toJson))),
      "messages" -> Json.fromFields(
        proto.messages.map(m => m.name -> Message.toJson(m))
      )
    )
  }

  def fromFreesFSchema[T](implicit T: Basis[AvroF, T]): Trans[MuF, AvroF, T] = Trans {
    case MuF.TNull()                => tNull()
    case MuF.TDouble()              => tDouble()
    case MuF.TFloat()               => tFloat()
    case MuF.TInt()                 => tInt()
    case MuF.TLong()                => tLong()
    case MuF.TBoolean()             => tBoolean()
    case MuF.TString()              => tString()
    case MuF.TByteArray()           => tBytes()
    case MuF.TNamedType(name)       => tNamedType(name)
    case MuF.TOption(value)         => tUnion(NonEmptyList(tNull[T]().embed, List(value)))
    case MuF.TEither(left, right)   => tUnion(NonEmptyList(left, List(right)))
    case MuF.TList(value)           => tArray(value)
    case MuF.TMap(_, value)         => tMap(value)
    case MuF.TGeneric(_, _)         => ??? // WAT
    case MuF.TContaining(_)         => ??? // TBD
    case MuF.TRequired(t)           => T.coalgebra(t)
    case MuF.TCoproduct(invariants) => TUnion(invariants)
    case MuF.TSum(name, fields)     => TEnum(name, none[String], Nil, none[String], fields)
    case MuF.TProduct(name, fields) =>
      TRecord(
        name,
        none[String],
        Nil,
        none[String],
        fields.map(f => Field(f.name, Nil, none[String], none[Order], f.tpe)))
  }

  def fromFreesFProtocol[T, U](proto: mu.Protocol[T])(implicit T: Basis[MuF, T], U: Basis[AvroF, U]): Protocol[U] = {
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
