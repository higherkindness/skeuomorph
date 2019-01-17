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

import cats.kernel.Eq
import cats.instances.list._
import cats.instances.string._
import cats.instances.int._
import cats.instances.tuple._
import cats.syntax.eq._

import qq.droste.macros.deriveTraverse

@deriveTraverse sealed trait ProtobufF[A]
object ProtobufF {
  @deriveTraverse final case class Field[A](name: String, tpe: A, position: Int, options: List[Option])
  object Field {
    implicit def fieldEq[T: Eq]: Eq[Field[T]] = Eq.instance {
      case (Field(n, t, p, o), Field(n2, t2, p2, o2)) => n === n2 && t === t2 && p === p2 && o === o2
    }
  }

  final case class Option(name: String, value: String)
  object Option {
    implicit val optionEq: Eq[Option] = Eq.instance {
      case (Option(n, v), Option(n2, v2)) => n === n2 && v === v2
    }
  }

  final case class TDouble[A]()                extends ProtobufF[A]
  final case class TFloat[A]()                 extends ProtobufF[A]
  final case class TInt32[A]()                 extends ProtobufF[A]
  final case class TInt64[A]()                 extends ProtobufF[A]
  final case class TUint32[A]()                extends ProtobufF[A]
  final case class TUint64[A]()                extends ProtobufF[A]
  final case class TSint32[A]()                extends ProtobufF[A]
  final case class TSint64[A]()                extends ProtobufF[A]
  final case class TFixed32[A]()               extends ProtobufF[A]
  final case class TFixed64[A]()               extends ProtobufF[A]
  final case class TSfixed32[A]()              extends ProtobufF[A]
  final case class TSfixed64[A]()              extends ProtobufF[A]
  final case class TBool[A]()                  extends ProtobufF[A]
  final case class TString[A]()                extends ProtobufF[A]
  final case class TBytes[A]()                 extends ProtobufF[A]
  final case class TNamedType[A](name: String) extends ProtobufF[A]
  final case class TRequired[A](value: A)      extends ProtobufF[A]
  final case class TOptional[A](value: A)      extends ProtobufF[A]
  final case class TRepeated[A](value: A)      extends ProtobufF[A]
  final case class TEnum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[Option],
      aliases: List[(String, Int)])
      extends ProtobufF[A]
  final case class TMessage[A](name: String, fields: List[Field[A]], reserved: List[List[String]]) extends ProtobufF[A]

  def double[A](): ProtobufF[A]                = TDouble()
  def float[A](): ProtobufF[A]                 = TFloat()
  def int32[A](): ProtobufF[A]                 = TInt32()
  def int64[A](): ProtobufF[A]                 = TInt64()
  def uint32[A](): ProtobufF[A]                = TUint32()
  def uint64[A](): ProtobufF[A]                = TUint64()
  def sint32[A](): ProtobufF[A]                = TSint32()
  def sint64[A](): ProtobufF[A]                = TSint64()
  def fixed32[A](): ProtobufF[A]               = TFixed32()
  def fixed64[A](): ProtobufF[A]               = TFixed64()
  def sfixed32[A](): ProtobufF[A]              = TSfixed32()
  def sfixed64[A](): ProtobufF[A]              = TSfixed64()
  def bool[A](): ProtobufF[A]                  = TBool()
  def string[A](): ProtobufF[A]                = TString()
  def bytes[A](): ProtobufF[A]                 = TBytes()
  def namedType[A](name: String): ProtobufF[A] = TNamedType(name)
  def required[A](value: A): ProtobufF[A]      = TRequired(value)
  def optional[A](value: A): ProtobufF[A]      = TOptional(value)
  def repeated[A](value: A): ProtobufF[A]      = TRepeated(value)
  def enum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[Option],
      aliases: List[(String, Int)]): ProtobufF[A] = TEnum(name, symbols, options, aliases)
  def message[A](name: String, fields: List[Field[A]], reserved: List[List[String]]): ProtobufF[A] =
    TMessage(name, fields, reserved)

  implicit def protobufEq[T: Eq]: Eq[ProtobufF[T]] = Eq.instance {
    case (TDouble(), TDouble())          => true
    case (TFloat(), TFloat())            => true
    case (TInt32(), TInt32())            => true
    case (TInt64(), TInt64())            => true
    case (TUint32(), TUint32())          => true
    case (TUint64(), TUint64())          => true
    case (TSint32(), TSint32())          => true
    case (TSint64(), TSint64())          => true
    case (TFixed32(), TFixed32())        => true
    case (TFixed64(), TFixed64())        => true
    case (TSfixed32(), TSfixed32())      => true
    case (TSfixed64(), TSfixed64())      => true
    case (TBool(), TBool())              => true
    case (TString(), TString())          => true
    case (TBytes(), TBytes())            => true
    case (TNamedType(n), TNamedType(n2)) => n === n2
    case (TRequired(v), TRequired(v2))   => v === v2
    case (TOptional(v), TOptional(v2))   => v === v2
    case (TRepeated(v), TRepeated(v2))   => v === v2

    case (TEnum(n, s, o, a), TEnum(n2, s2, o2, a2)) =>
      n === n2 && s === s2 && o === o2 && a === a2
    case (TMessage(n, f, r), TMessage(n2, f2, r2)) => n === n2 && f === f2 && r === r2

    case _ => false
  }

}
