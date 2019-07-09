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

  def schemaWithName[T: Basis[JsonSchemaF, ?]](implicit codecs: Printer[Codecs]): Printer[(String, T)] = Printer {
    case (name, t) if (isBasicType(t)) => typeAliasDef(schema[T]()).print((name, t, none))
    case (name, t)                     => schema[T](name.some).print(t)
  }

  protected[openapi] def schema[T: Basis[JsonSchemaF, ?]](name: Option[String] = None)(
      implicit codecs: Printer[Codecs]): Printer[T] = {

    val listDef: Printer[String] = konst("List[") *< string >* konst("]")

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
          if (properties.isEmpty)
            s"type $name = io.circe.Json"
          else
            caseClassWithCodecsDef.print(
              (
                name,
                properties
                  .map(x => x.name -> Tpe[T](x.tpe))
                  .map {
                    case (name, tpe) =>
                      if (required.contains(name))
                        name -> tpe
                      else
                        name -> tpe.copy(required = false)
                  }))
        case (ArrayF(x), Some(name)) =>
          typeAliasDef(listDef).print((name, x, (List.empty, ListCodecs(name)).some))
        case (ArrayF(x), _) => listDef.print(x)
        case (EnumF(fields), Some(name)) =>
          sealedTraitDef.print(name -> fields)
        case (ReferenceF(componentsRegex(ref)), _) => ref
        case (ReferenceF(ref), _)                  => ref
      }
    }

    Printer(scheme.cata(algebra))
  }

  def isBasicType[T: Basis[JsonSchemaF, ?]](t: T): Boolean = {
    import JsonSchemaF._
    val algebra: Algebra[JsonSchemaF, Boolean] = Algebra {
      case ObjectF(_, _) => false
      case EnumF(_)      => false
      case ArrayF(_)     => false
      case ReferenceF(_) => false
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

  sealed trait Codecs
  final case class CaseClassCodecs(name: String)                  extends Codecs
  final case class ListCodecs(name: String)                       extends Codecs
  final case class EnumCodecs(name: String, values: List[String]) extends Codecs

  def model[T: Basis[JsonSchemaF, ?]](implicit codecs: Printer[Codecs]): Printer[OpenApi[T]] =
    objectDef(sepBy(schemaWithName, "\n")).contramap { x =>
      ("models", List.empty, x.components.toList.flatMap(_.schemas))
    }

  private def caseObjectDef: Printer[(String, String)] =
    (konst("final case object ") *< string >* konst(" extends "), string).contramapN(identity)

  private def sealedTraitCompanionObjectDef(
      implicit codecs: Printer[Codecs]): Printer[(List[(String, String)], Codecs)] =
    (sepBy(space *< space *< caseObjectDef, "\n") >* newLine, codecs).contramapN(identity)

  private def sealedTraitDef(implicit codecs: Printer[Codecs]): Printer[(String, List[String])] =
    (konst("sealed trait ") *< string >* newLine, objectDef(sealedTraitCompanionObjectDef))
      .contramapN {
        case (name, fields) =>
          (name, (name, List.empty, (fields.map(_ -> name), EnumCodecs(name, fields))))
      }

  def caseClassDef[T: Basis[JsonSchemaF, ?]]: Printer[(String, List[(String, Tpe[T])])] =
    (konst("final case class ") *< string, konst("(") *< sepBy(argumentDef[T], ", ") >* konst(")")).contramapN {
      case (x, y) => x -> y.map { case (x, y) => Var[T](x, y) }
    }

  def caseClassWithCodecsDef[T: Basis[JsonSchemaF, ?], A](
      implicit codecs: Printer[Codecs]): Printer[(String, List[(String, Tpe[T])])] =
    (caseClassDef[T], optional(newLine *< objectDef(codecs))).contramapN { x =>
      ((x._1 -> x._2), (x._1, List.empty[PackageName], CaseClassCodecs(x._1)).some)
    }

  def typeAliasDef[T](typeSchemaDef: Printer[T])(
      implicit codecs: Printer[Codecs]): Printer[(String, T, Option[(List[PackageName], Codecs)])] =
    (konst("type ") *< string >* konst(" = "), typeSchemaDef, optional(newLine *< objectDef(codecs))).contramapN {
      case (name, tpe, codecInfo) =>
        (name, tpe, codecInfo.map { case (x, y) => (name, x, y) })
    }

  def objectDef[A](body: Printer[A]): Printer[(String, List[PackageName], A)] =
    (
      konst("object ") *< string >* konst(" {") *< newLine,
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
      x => {
        import Printer.avoid._
        Printer(Optimize.namedTypes[T](normalize(tpe.description)) >>> schema(none).print)
          .print(x)
          .capitalize
      }
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
