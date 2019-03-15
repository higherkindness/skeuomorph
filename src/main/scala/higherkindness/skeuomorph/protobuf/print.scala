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

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.uast.derivation._
import higherkindness.skeuomorph.protobuf.types._
import higherkindness.skeuomorph.compdata.Ann
import qq.droste._

object print {

  def printOption(o: OptionValue): String = s"${o.name} = ${o.value}"

  def printSchema[T: Basis[Type, ?]]: Printer[T] = {
    val algebra: Algebra[Type, String] = Algebra {
      case InjNull(_)                     => "null"
      case InjDouble(_)                   => "double"
      case InjFloat(_)                    => "float"
      case InjInt32(_)                    => "int32"
      case InjInt64(_)                    => "int64"
      case InjUint32(_)                   => "uint32"
      case InjUint64(_)                   => "uint64"
      case InjSint32(_)                   => "sint32"
      case InjSint64(_)                   => "sint64"
      case InjFixed32(_)                  => "fixed32"
      case InjFixed64(_)                  => "fixed64"
      case InjSfixed32(_)                 => "sfixed32"
      case InjSfixed64(_)                 => "sfixed64"
      case InjBoolean(_)                  => "bool"
      case InjString(_)                   => "string"
      case InjByteArray(_)                => "bytes"
      case InjNamedType(TNamedType(name)) => name
      case InjList(TList(value))          => s"repeated $value"
      case InjMap(TMap(key, value))       => s"map<$key, $value>"

      case InjFileDescriptor(TFileDescriptor(values, _, packageName)) =>
        s"package $packageName; \n ${values.mkString("\n")}"

      case InjProtoEnum(TProtoEnum(name, symbols, options, aliases)) =>
        val printOptions = options.map(o => s"\toption ${o.name} = ${o.value};").mkString("\n")
        val printSymbols = symbols.map({ case (s, i) => s"\t$s = $i;" }).mkString("\n")
        val printAliases = aliases.map({ case (s, i) => s"\t$s = $i;" }).mkString("\n")
        s"""
      |enum $name {
      |$printOptions
      |$printSymbols
      |$printAliases
      |}
      """.stripMargin

      case InjMessage(Ann(TRecord(name, fields), reserved)) =>
        val printReserved: String = reserved
          .map(l => s"reserved " + l.mkString(start = "", sep = ", ", end = ";"))
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
                s"$tpe $name = ${position}${printOptions(options)};"
              case FieldF.InjSimpleField(Field(_, tpe)) =>
                s"$tpe"
            }
            .mkString("\n  ")

        s"""
      |message $name {
      |  $printReserved
      |  $printFields
      |}
      """.stripMargin

      case InjOneOf(TOneOf(name, fields)) =>
        val printFields =
          fields
            .collect {
              case FieldF.InjProtobufField(Ann(Field(name, tpe), (position, _, _, _))) => (name, tpe, position)
            }
            .map {
              case (name, tpe, position) =>
                s"$tpe $name = $position;"
            }
            .toList
            .mkString("\n  ")

        s"""
      |oneof $name {
      |  $printFields
      |}
      """.stripMargin
    }

    Printer(scheme.cata(algebra))
  }
}
