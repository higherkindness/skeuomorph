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
package protobuf

import cats.Functor
import qq.droste.Algebra

sealed trait ProtobufF[A]
object ProtobufF {
  final case class Field[A](name: String, tpe: A, position: Int, options: List[Option])
  final case class Option(name: String, value: String)

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

  implicit val protobufFunctor: Functor[ProtobufF] = new Functor[ProtobufF] {
    def map[A, B](fa: ProtobufF[A])(f: A => B): ProtobufF[B] = fa match {
      case TDouble()                              => TDouble()
      case TFloat()                               => TFloat()
      case TInt32()                               => TInt32()
      case TInt64()                               => TInt64()
      case TUint32()                              => TUint32()
      case TUint64()                              => TUint64()
      case TSint32()                              => TSint32()
      case TSint64()                              => TSint64()
      case TFixed32()                             => TFixed32()
      case TFixed64()                             => TFixed64()
      case TSfixed32()                            => TSfixed32()
      case TSfixed64()                            => TSfixed64()
      case TBool()                                => TBool()
      case TString()                              => TString()
      case TBytes()                               => TBytes()
      case TNamedType(name)                       => TNamedType(name)
      case TRequired(value)                       => TRequired(f(value))
      case TOptional(value)                       => TOptional(f(value))
      case TRepeated(value)                       => TRepeated(f(value))
      case TEnum(name, symbols, options, aliases) => TEnum(name, symbols, options, aliases)
      case TMessage(name, fields, reserved) =>
        TMessage(
          name,
          fields.map(field => field.copy(tpe = f(field.tpe))),
          reserved
        )

    }
  }

  def printOption(o: Option): String = s"${o.name} = ${o.value}"

  def render: Algebra[ProtobufF, String] = Algebra {
    case TDouble()        => "double"
    case TFloat()         => "float"
    case TInt32()         => "int32"
    case TInt64()         => "int64"
    case TUint32()        => "uint32"
    case TUint64()        => "uint64"
    case TSint32()        => "sint32"
    case TSint64()        => "sint64"
    case TFixed32()       => "fixed32"
    case TFixed64()       => "fixed64"
    case TSfixed32()      => "sfixed32"
    case TSfixed64()      => "sfixed64"
    case TBool()          => "bool"
    case TString()        => "string"
    case TBytes()         => "bytes"
    case TNamedType(name) => name

    case TRequired(value) => s"required $value"
    case TOptional(value) => s"optional $value"
    case TRepeated(value) => s"repeated $value"

    case TEnum(name, symbols, options, aliases) =>
      val printOptions = options.map(o => s"option ${o.name} = ${o.value}").mkString("\n")
      val printSymbols = symbols.map({ case (s, i) => s"$s = $i;" }).mkString("\n")
      val printAliases = aliases.map({ case (s, i) => s"$s = $i;" }).mkString("\n")
      s"""
      |enum $name {
      |  $printOptions
      |  $printSymbols
      |  $printAliases
      |}
      """.stripMargin
    case TMessage(name, fields, reserved) =>
      val printReserved = reserved.map(l => s"reserved " + l.mkString(", ")).mkString("\n  ")
      def printOptions(options: List[Option]) =
        if (options.isEmpty)
          ""
        else
          options.map(printOption).mkString(start = " [", sep = ", ", end = "]")

      val printFields =
        fields
          .map(f => s"${f.tpe} ${f.name} = ${f.position}${printOptions(f.options)};")
          .mkString("\n  ")
      s"""
      |message $name {
      |  $printReserved
      |  $printFields
      |}
      """.stripMargin
  }
}
