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
import higherkindness.skeuomorph.mu.muTraverse
import higherkindness.skeuomorph.mu.SerializationType
import higherkindness.skeuomorph.uast.types._

import cats.implicits._
import cats.data.NonEmptyList
import io.circe.Json
import org.apache.avro.{Schema, Protocol => AvroProtocol}
import higherkindness.droste._
import higherkindness.droste.syntax.embed._

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

  def fromMuSchema[T](implicit T: Basis[Type, T]): TransM[Option, mu.Type, Type, T] = TransM {
    case mu.InjNull(_)                      => `null`[Type, T].some
    case mu.InjDouble(_)                    => double[Type, T].some
    case mu.InjFloat(_)                     => float[Type, T].some
    case mu.InjInt(_)                       => int[Type, T].some
    case mu.InjLong(_)                      => long[Type, T].some
    case mu.InjBoolean(_)                   => boolean[Type, T].some
    case mu.InjString(_)                    => string[Type, T].some
    case mu.InjByteArray(_)                 => byteArray[Type, T].some
    case mu.InjNamedType(TNamedType(name))  => namedType[Type, T](name).some
    case mu.InjOption(TOption(value))       => union[Type, T](NonEmptyList(`null`[Type, T].embed, List(value))).some
    case mu.InjEither(TEither(left, right)) => union[Type, T](NonEmptyList(left, List(right))).some
    case mu.InjList(TList(value))           => list[Type, T](value).some
    case mu.InjMap(TMap(_, value))          => map[Type, T](string[Type, T].embed, value).some
    case mu.InjGeneric(_)                   => none[Type[T]]
    case mu.InjUnion(TUnion(invariants))    => union[Type, T](invariants).some
    case mu.InjEnum(TEnum(name, fields)) =>
      enum[Type, T](name, fields).some
    case mu.InjRecord(TRecord(name, fields)) =>
      avroRecord[Type, T](name, none[String], List.empty[String], none[String], fields).some
  }

  def fromFreesFProtocol[T, U](
      protocol: mu.Protocol[T])(implicit T: Basis[mu.Type, T], U: Basis[Type, U]): Option[Protocol[U]] = {
    val fromMu: T => Option[U] = scheme.cataM(fromMuSchema.algebra)
    val services: Option[List[Message[U]]] =
      protocol.services
        .filter(_.serializationType == SerializationType.Avro)
        .flatTraverse { s =>
          s.operations.traverse[Option, Message[U]](op =>
            (op.name.some, fromMu(op.request.tpe), fromMu(op.response.tpe)).mapN(Message.apply))
        }

    (services, protocol.declarations.traverse(fromMu)).mapN(
      (s, decls) =>
        Protocol(
          protocol.name,
          protocol.pkg,
          decls,
          s
      ))
  }
}
