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

package higherkindness.skeuomorph.openapi.client.http4s

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer._
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.openapi._
import higherkindness.skeuomorph.openapi.print._
import client.http4s.print._
import cats.implicits._
import qq.droste._
import cats.implicits._

import higherkindness.skeuomorph.openapi.print.Codecs

package object circe {
  private val http4sPackages =
    List("org.http4s.{EntityEncoder, EntityDecoder}", "org.http4s.circe._", "cats.Applicative", "cats.effect.Sync").map(
      PackageName.apply)

  private val packages =
    List("io.circe._", "io.circe.generic.semiauto._").map(PackageName.apply) ++ http4sPackages

  implicit def circeCodecsPrinter[T: Basis[JsonSchemaF, ?]]: Printer[Codecs[T]] =
    (
      konst("object ") *< tpe[T] >* konst(" {") *< newLine,
      sepBy(space *< space *< importDef, "\n") >* newLine,
      optional(space *< space *< circeEncoder[T] >* newLine),
      optional(space *< space *< circeDecoder[T] >* newLine),
      space *< space *< entityEncoder[T] >* newLine,
      space *< space *< entityEncoder[T] >* newLine,
      space *< space *< entityDecoder[T] >* newLine >* konst("}"))
      .contramapN { x =>
        val tpe     = Tpe[T](x.name)
        val default = x.name -> tpe
        val (imports, tpeWithName) =
          if (isArray(x.tpe))
            http4sPackages -> none
          else
            packages -> default.some
        (tpe, imports, tpeWithName, tpeWithName, default, s"Option${x.name}" -> tpe.copy(required = false), default)
      }

  implicit def http4sCodecsPrinter[T: Basis[JsonSchemaF, ?]]: Printer[EntityCodecs[T]] =
    (
      sepBy(space *< space *< importDef, "\n") >* newLine,
      space *< space *< entityEncoder[T] >* newLine,
      space *< space *< entityDecoder[T] >* newLine)
      .contramapN { x =>
        val y = x.name -> Tpe[T](x.name)
        (packages, y, y)
      }

  def circeDecoder[T: Basis[JsonSchemaF, ?]]: Printer[(String, Tpe[T])] =
    (
      konst("implicit val ") *< string >* konst("Decoder: "),
      konst("Decoder[") *< tpe[T] >* konst("] = "),
      konst("deriveDecoder[") *< tpe[T] >* konst("]"))
      .contramapN(x => (x._1, x._2, x._2))

  def circeEncoder[T: Basis[JsonSchemaF, ?]]: Printer[(String, Tpe[T])] =
    (
      konst("implicit val ") *< string >* konst("Encoder: "),
      konst("Encoder[") *< tpe[T] >* konst("] = "),
      konst("deriveEncoder[") *< tpe[T] >* konst("]"))
      .contramapN(x => (x._1, x._2, x._2))

  def entityDecoder[T: Basis[JsonSchemaF, ?]]: Printer[(String, Tpe[T])] =
    (
      konst("implicit def ") *< string >* konst("EntityDecoder[F[_]:Sync]: "),
      konst("EntityDecoder[F, ") *< tpe[T] >* konst("] = "),
      konst("jsonOf[F, ") *< tpe[T] >* konst("]"))
      .contramapN(x => (x._1, x._2, x._2))

  def entityEncoder[T: Basis[JsonSchemaF, ?]]: Printer[(String, Tpe[T])] =
    (
      konst("implicit def ") *< string >* konst("EntityEncoder[F[_]:Applicative]: "),
      konst("EntityEncoder[F, ") *< tpe[T] >* konst("] = "),
      konst("jsonEncoderOf[F, ") *< tpe[T] >* konst("]"))
      .contramapN(x => (x._1, x._2, x._2))

}
