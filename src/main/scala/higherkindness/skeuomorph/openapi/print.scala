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

package higherkindness.skeuomorph.openapi

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer._
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.openapi.JsonSchemaF.{string => _, _}
import cats.implicits._

import qq.droste._

object print {
  import higherkindness.skeuomorph.openapi.schema.Components

  val componentsRegex = """#/components/schemas/(.+)""".r

  def schema[T: Basis[JsonSchemaF, ?]](name: Option[String] = None): Printer[T] = {
    val algebra: Algebra[JsonSchemaF, String] = Algebra { x =>
      import JsonSchemaF._
      (x, name) match {
        case (IntegerF(), _)  => "Int"
        case (LongF(), _)     => "Long"
        case (FloatF(), _)    => "Float"
        case (DoubleF(), _)   => "Double"
        case (StringF(), _)   => "String"
        case (ByteF(), _)     => "Array[Byte]"
        case (BinaryF(), _)   => "List[Boolean]"
        case (BooleanF(), _)  => "Boolean"
        case (DateF(), _)     => "java.time.LocalDate"
        case (DateTimeF(), _) => "java.time.ZonedDateTime"
        case (PasswordF(), _) => "String"
        case (ObjectF(properties, required), Some(name)) =>
          val (requiredFields, optionalFields) = properties.partition(x => required.contains(x.name))
          val comma                            = if (requiredFields.nonEmpty && optionalFields.nonEmpty) ", " else ""
          val printRequired                    = requiredFields.map(x => s"${x.name}: ${x.tpe}").mkString(", ")
          val printOptional                    = optionalFields.map(x => s"${x.name}: Option[${x.tpe}]").mkString(", ")
          s"final case class $name($printRequired$comma$printOptional)"
        case (ArrayF(x), _) => s"List[$x]"
        case (EnumF(fields), Some(name)) =>
          val printFields = fields.map(f => s"final case object ${f.capitalize} extends $name").mkString("\n  ")
          s"""
      |sealed trait $name
      |object $name {
      |  $printFields
      |}""".stripMargin
        case (ReferenceF(componentsRegex(ref)), _) => ref
        case (ReferenceF(ref), _)                  => ref
      }
    }

    Printer(scheme.cata(algebra))
  }

  def isBasic[T: Basis[JsonSchemaF, ?]](t: T): Boolean = {
    import JsonSchemaF._
    val algebra: Algebra[JsonSchemaF, Boolean] = Algebra {
      case ObjectF(_, _) => false
      case EnumF(_)      => false
      case _             => true
    }
    scheme.cata(algebra).apply(t)
  }

  def schemaPair[T: Basis[JsonSchemaF, ?]]: Printer[(String, T)] = Printer {
    case (name, tpe) =>
      if (isBasic(tpe)) s"type $name = ${schema().print(tpe)}" else schema(name.some).print(tpe)
  }

  def model[T: Basis[JsonSchemaF, ?]]: Printer[Components[T]] =
    (
      (konst("object models {") >* newLine >* space >* space) *< sepBy(schemaPair, "\n  ") >* (newLine >* konst("}"))
    ).contramap(_.schemas.toList)

}
