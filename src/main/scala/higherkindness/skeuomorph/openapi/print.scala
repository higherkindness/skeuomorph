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
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi.JsonSchemaF.{string => _, _}
import cats.implicits._
import cats.Show

import qq.droste._

object print {
  import higherkindness.skeuomorph.openapi.schema.OpenApi

  val componentsRegex = """#/components/schemas/(.+)""".r

  def schemaWithName[T: Basis[JsonSchemaF, ?]]: Printer[(String, T)] = Printer {
    case (x, t) =>
      schema[T](x.some).print(t)
  }

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

  def isArray[T: Basis[JsonSchemaF, ?]](t: T): Boolean = {
    import JsonSchemaF._
    val algebra: Algebra[JsonSchemaF, Boolean] = Algebra {
      case ArrayF(_) => true
      case _         => false
    }
    scheme.cata(algebra).apply(t)
  }

  def schemaPair[T: Basis[JsonSchemaF, ?]]: Printer[(String, T)] = Printer {
    case (name, tpe) =>
      if (isBasic(tpe)) s"type $name = ${schema().print(tpe)}" else schema(name.some).print(tpe)
  }

  final case class Codecs[T](name: String, tpe: T)

  def model[T: Basis[JsonSchemaF, ?]](implicit codecs: Printer[Codecs[T]]): Printer[OpenApi[T]] =
    (
      konst("object models {") *< newLine *< sepBy(space *< space *< schemaPair, "\n") >* newLine,
      sepBy(space *< space *< codecs, "\n") >* newLine *< konst("}")
    ).contramapN { x =>
      val y = x.components.toList.flatMap(_.schemas)
      y -> y.map((Codecs.apply[T] _).tupled)
    }

  def caseClassDef[T: Basis[JsonSchemaF, ?], A]: Printer[(Tpe[T], List[(String, Tpe[T])])] =
    (konst("final case class ") *< tpe[T], konst("(") *< sepBy(argumentDef[T], ", ") >* konst(")")).contramapN {
      case (x, y) => x -> y.map { case (x, y) => Var[T](x, y) }
    }

  def objectDef[T: Show, A](body: Printer[A]): Printer[(T, List[PackageName], A)] =
    (
      konst("object ") *< show[T] >* konst(" {") *< newLine,
      sepBy(importDef, "\n") >* newLine,
      body >* newLine *< konst("}")).contramapN(identity)

  def normalize(value: String): String = value.split(" ").map(_.filter(_.isLetter).capitalize).mkString

  def divBy[A, B](p1: Printer[A], p2: Printer[B])(sep: Printer[Unit]): Printer[(A, B)] =
    (p1, sep, p2).contramapN[(A, B)] { case (x, y) => (x, (), y) }

  final case class Tpe[T](tpe: Either[String, T], required: Boolean, description: String)
  object Tpe {
    def unit[T]: Tpe[T]                = Tpe("Unit".asLeft, true, "Unit")
    def apply[T](name: String): Tpe[T] = Tpe(name.asLeft, true, name)
    def apply[T](tpe: T, required: Boolean, description: String): Tpe[T] =
      Tpe(tpe.asRight, required, description)

    def name[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): String = tpe.tpe.fold(
      identity,
      x =>
        Printer(Optimize.namedTypes[T](normalize(tpe.description)) >>> schema(none).print)
          .print(x)
          .capitalize
    )
    def option[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): Either[String, String] =
      if (tpe.required)
        name(tpe).asRight
      else
        name(tpe).asLeft

  }

  def tpe[T: Basis[JsonSchemaF, ?]]: Printer[Tpe[T]] =
    ((konst("Option[") *< string >* konst("]")) >|< string)
      .contramap(Tpe.option[T])

  def importDef: Printer[PackageName] =
    (konst("import ") *< show[PackageName]).contramap(identity)

  final case class PackageName(value: String) extends AnyVal
  object PackageName {
    implicit val packageNameShow: Show[PackageName] = Show.show(_.value)
  }

  final case class Var[T](name: String, tpe: Tpe[T])
  object Var {
    def tpe[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): Var[T] = Var(decapitalize(Tpe.name(tpe)), tpe)
    def apply[T](name: String, tpe: T, required: Boolean, description: String): Var[T] =
      Var(decapitalize(name), Tpe(tpe.asRight, required, description))
  }
  def argumentDef[T: Basis[JsonSchemaF, ?]]: Printer[Var[T]] =
    (
      string >* konst(": "),
      tpe
    ).contramapN(x => x.name -> x.tpe)

  def un[A, B, C, D](pair: ((A, B), (C, D))): (A, B, C, D) = (pair._1._1, pair._1._2, pair._2._1, pair._2._2)
  def un[A, C, D](pair: (A, (C, D))): (A, C, D)            = (pair._1, pair._2._1, pair._2._2)
  def duplicate[A, B](pair: (A, B)): ((A, A), (B, B))      = (pair._1 -> pair._1, pair._2 -> pair._2)
  def second[A, B, C](pair: (A, B))(f: B => C): (A, C)     = (pair._1, f(pair._2))
  def flip[A, B](pair: (A, B)): (B, A)                     = (pair._2, pair._1)
  def decapitalize(s: String)                              = if (s.isEmpty) s else s(0).toLower + s.substring(1)

}
