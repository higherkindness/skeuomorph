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

package higherkindness.skeuomorph.protobuf

import higherkindness.skeuomorph.mu
import higherkindness.skeuomorph.mu.{MuF, SerializationType}
import qq.droste._

final case class Protocol[A](
    name: String,
    `package`: scala.Option[String],
    types: List[A],
    services: List[Protocol.Service[A]]
)

object Protocol {
  import ProtobufF._

  final case class Service[A](name: String, methods: List[Method[A]])
  final case class Method[A](name: String, input: A, output: A)

  def fromMuFSchema[T](implicit T: Basis[ProtobufF, T]): Trans[MuF, ProtobufF, T] = Trans {
    case MuF.TDouble()          => TDouble()
    case MuF.TFloat()           => TFloat()
    case MuF.TInt()             => TInt32()
    case MuF.TLong()            => TInt64()
    case MuF.TBoolean()         => TBool()
    case MuF.TString()          => TString()
    case MuF.TByteArray()       => TBytes()
    // case MuF.TList(value)       => TRepeated(value)
    case MuF.TSum(name, fields) => TEnum(name, fields.zipWithIndex, Nil, Nil) // TODO verify .zipWithIndex
    case MuF.TRequired(value)   => T.coalgebra(value) // TODO
    case MuF.TNamedType(name)   => TNamedType(name)
    case MuF.TProduct(name, fields) =>
      TMessage(name, fields.zipWithIndex.map { case (f, i) => Field(f.name, f.tpe, i, Nil) }, Nil)
    case MuF.TNull()        => ???
    case MuF.TCoproduct(_)  => ??? // TOneOf(invariants)
    case MuF.TOption(_)     => ??? // TOption(value)
    case MuF.TEither(_, _)  => ??? // TOneOf(left, right)
    case MuF.TMap(_)        => ??? // TMap(value)
    case MuF.TGeneric(_, _) => ??? //
  }

  def fromMuFProtocol[T, U](proto: mu.Protocol[T])(implicit T: Basis[MuF, T], U: Basis[ProtobufF, U]): Protocol[U] = {
    def fromMu: T => U = scheme.cata(fromMuFSchema.algebra)
    val services = proto.services
      .filter(_.serializationType == SerializationType.Protobuf)
      .map { s =>
        Service(s.name, s.operations.map(o => Method(o.name, fromMu(o.request), fromMu(o.response))))
      }

    Protocol(
      proto.name,
      proto.pkg,
      proto.declarations.map(fromMu),
      services
    )
  }
}
