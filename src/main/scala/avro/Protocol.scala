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

package skeuomorph
package avro

import org.apache.avro.Schema
import org.apache.avro.{Protocol => AvroProtocol}
import scala.collection.JavaConverters._

import cats.syntax.option._
import cats.data.NonEmptyList
import qq.droste._
import qq.droste.syntax.all._
import freestyle.{FreesF, SerializationType}

case class Protocol[A](
    name: String,
    namespace: Option[String],
    types: List[A],
    messages: List[Protocol.Message[A]]
)

object Protocol {
  import AvroF._

  case class Message[A](name: String, request: A, response: A)

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

  def fromFreesFSchema[T](implicit T: Basis[AvroF, T]): Trans[FreesF, AvroF, T] = Trans {
    case FreesF.TNull()                => tNull()
    case FreesF.TDouble()              => tDouble()
    case FreesF.TFloat()               => tFloat()
    case FreesF.TInt()                 => tInt()
    case FreesF.TLong()                => tLong()
    case FreesF.TBoolean()             => tBoolean()
    case FreesF.TString()              => tString()
    case FreesF.TByteArray()           => tBytes()
    case FreesF.TNamedType(name)       => tNamedType(name)
    case FreesF.TOption(value)         => tUnion(NonEmptyList(tNull[T]().embed, List(value)))
    case FreesF.TList(value)           => tArray(value)
    case FreesF.TMap(value)            => tMap(value)
    case FreesF.TGeneric(_, _)         => ??? // WAT
    case FreesF.TRequired(t)           => T.coalgebra(t)
    case FreesF.TCoproduct(invariants) => TUnion(invariants)
    case FreesF.TSum(name, fields)     => TEnum(name, none[String], Nil, none[String], fields)
    case FreesF.TProduct(name, fields) =>
      TRecord(
        name,
        none[String],
        Nil,
        none[String],
        fields.map(f => Field(f.name, Nil, none[String], none[Order], f.tpe)))
  }

  def fromFreesFProtocol[T, U](
      proto: freestyle.Protocol[T])(implicit T: Basis[FreesF, T], U: Basis[AvroF, U]): Protocol[U] = {
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
