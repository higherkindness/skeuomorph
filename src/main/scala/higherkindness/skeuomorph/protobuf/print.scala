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

import higherkindness.skeuomorph.Printer
import qq.droste._

object print {

  import ProtobufF._

  def printOption(o: Option): String = s"${o.name} = ${o.value}"

  def printSchema[T: Basis[ProtobufF, ?]]: Printer[T] = {
    val algebra: Algebra[ProtobufF, String] = Algebra {
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

      case TRepeated(value) => s"repeated $value"

      case TFileDescriptor(_, _, _) => s"" // TODO: Figure this out

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

    Printer(scheme.cata(algebra))
  }
}
