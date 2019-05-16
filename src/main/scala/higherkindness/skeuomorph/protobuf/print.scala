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

package higherkindness.skeuomorph
package protobuf

import cats.syntax.show._
import cats.instances.string._

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.uast.{derivation, Delay}
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.compdata.Ann

import Function.{const => κ}

object print {

  def printOption(o: OptionValue): String = s"${o.name} = ${o.value}"

  implicit val printNull: Delay[Printer, TNull] = new Delay[Printer, TNull] {
    def apply[A](x: Printer[A]) = Printer(κ("null"))
  }
  implicit val printDouble: Delay[Printer, TDouble] = new Delay[Printer, TDouble] {
    def apply[A](x: Printer[A]) = Printer(κ("double"))
  }
  implicit val printFloat: Delay[Printer, TFloat] = new Delay[Printer, TFloat] {
    def apply[A](x: Printer[A]) = Printer(κ("float"))
  }
  implicit val printInt32: Delay[Printer, TInt32] = new Delay[Printer, TInt32] {
    def apply[A](x: Printer[A]) = Printer(κ("int32"))
  }
  implicit val printInt64: Delay[Printer, TInt64] = new Delay[Printer, TInt64] {
    def apply[A](x: Printer[A]) = Printer(κ("int64"))
  }
  implicit val printUint32: Delay[Printer, TUInt32] = new Delay[Printer, TUInt32] {
    def apply[A](x: Printer[A]) = Printer(κ("uint32"))
  }
  implicit val printUint64: Delay[Printer, TUInt64] = new Delay[Printer, TUInt64] {
    def apply[A](x: Printer[A]) = Printer(κ("uint64"))
  }
  implicit val printSint32: Delay[Printer, TSInt32] = new Delay[Printer, TSInt32] {
    def apply[A](x: Printer[A]) = Printer(κ("sint32"))
  }
  implicit val printSint64: Delay[Printer, TSInt64] = new Delay[Printer, TSInt64] {
    def apply[A](x: Printer[A]) = Printer(κ("sint64"))
  }
  implicit val printFixed32: Delay[Printer, TFixed32] = new Delay[Printer, TFixed32] {
    def apply[A](x: Printer[A]) = Printer(κ("fixed32"))
  }
  implicit val printFixed64: Delay[Printer, TFixed64] = new Delay[Printer, TFixed64] {
    def apply[A](x: Printer[A]) = Printer(κ("fixed64"))
  }
  implicit val printSfixed32: Delay[Printer, TSFixed32] = new Delay[Printer, TSFixed32] {
    def apply[A](x: Printer[A]) = Printer(κ("sfixed32"))
  }
  implicit val printSfixed64: Delay[Printer, TSFixed64] = new Delay[Printer, TSFixed64] {
    def apply[A](x: Printer[A]) = Printer(κ("sfixed64"))
  }
  implicit val printBoolean: Delay[Printer, TBoolean] = new Delay[Printer, TBoolean] {
    def apply[A](x: Printer[A]) = Printer(κ("bool"))
  }
  implicit val printString: Delay[Printer, TString] = new Delay[Printer, TString] {
    def apply[A](x: Printer[A]) = Printer(κ("string"))
  }
  implicit val printByteArray: Delay[Printer, TByteArray] = new Delay[Printer, TByteArray] {
    def apply[A](x: Printer[A]) = Printer(κ("bytes"))
  }
  implicit val printNamedType: Delay[Printer, TNamedType] = new Delay[Printer, TNamedType] {
    def apply[A](x: Printer[A]) = Printer { case TNamedType(name) => name }
  }
  implicit val printList: Delay[Printer, TList] = new Delay[Printer, TList] {
    def apply[A](x: Printer[A]) = Printer { case TList(value) => show"repeated ${x.print(value)}" }
  }
  implicit val printMap: Delay[Printer, TMap] = new Delay[Printer, TMap] {
    def apply[A](x: Printer[A]) = Printer { case TMap(key, value) => show"map<${x.print(key)}, ${x.print(value)}>" }
  }
  implicit val printFileDescriptor: Delay[Printer, TFileDescriptor] = new Delay[Printer, TFileDescriptor] {
    def apply[A](x: Printer[A]) = Printer {
      case TFileDescriptor(values, _, packageName) =>
        show"package $packageName; \n ${values.map(x.print).mkString("\n")}"
    }
  }
  implicit val printProtoEnum: Delay[Printer, TProtoEnum] = new Delay[Printer, TProtoEnum] {
    def apply[A](x: Printer[A]) = Printer {
      case Ann(TEnum(name, symbolNames), annotations.EnumAnnotation(symbolNumbers, options, aliases)) =>
        val printOptions = options.map(o => s"\toption ${o.name} = ${o.value};").mkString("\n")
        val printSymbols = symbolNames.zip(symbolNumbers).map({ case (s, i) => s"\t$s = $i;" }).mkString("\n")
        val printAliases = aliases.map({ case (s, i) => s"\t$s = $i;" }).mkString("\n")
        show"""
      |enum $name {
      |$printOptions
      |$printSymbols
      |$printAliases
      |}
      """.stripMargin
    }
  }
  implicit val printMessage: Delay[Printer, TMessage] = new Delay[Printer, TMessage] {
    def apply[A](x: Printer[A]) = Printer {
      case Ann(TRecord(name, fields), annotations.Reserved(reserved)) =>
        val printReserved: String = reserved
          .map(l => "reserved " + l.mkString(start = "", sep = ", ", end = ";"))
          .mkString("\n  ")
        def printOptions(options: List[OptionValue]) =
          if (options.isEmpty)
            ""
          else
            options.map(printOption).mkString(start = " [", sep = ", ", end = "]")

        val printFields =
          fields
            .map {
              case FieldF.InjProtobufField(Ann(Field(name, tpe), (position, options, _, _))) =>
                s"${x.print(tpe)} $name = ${position}${printOptions(options)};"
              case FieldF.InjSimpleField(Field(_, tpe)) =>
                s"${x.print(tpe)}"
            }
            .mkString("\n  ")

        s"""
      |message $name {
      |  $printReserved
      |  $printFields
      |}
      """.stripMargin
    }
  }
  implicit val printOneOf: Delay[Printer, TOneOf] = new Delay[Printer, TOneOf] {
    def apply[A](x: Printer[A]) = Printer {
      case TOneOf(name, fields) =>
        val printFields =
          fields
            .collect {
              case FieldF.InjProtobufField(Ann(Field(name, tpe), (position, _, _, _))) => (name, tpe, position)
            }
            .map {
              case (name, tpe, position) =>
                s"${x.print(tpe)} $name = $position;"
            }
            .toList
            .mkString("\n  ")

        s"""
      |oneof $name {
      |  $printFields
      |}
      """.stripMargin
    }
  }

  val printSchema: Delay[Printer, Type] = derivation.copkPrinter[Types]
}
