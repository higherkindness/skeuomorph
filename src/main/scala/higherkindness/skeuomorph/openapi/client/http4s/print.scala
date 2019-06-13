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
import higherkindness.skeuomorph.openapi.client.print._
import cats.implicits._
import qq.droste._

object print {
  import schema._

  object v20 {

    val imports = List(
      "cats.effect._",
      "cats.syntax.functor._",
      "cats.syntax.either._",
      "cats.syntax.show._",
      "cats.implicits.catsStdShowForLong",
      "org.http4s._",
      "org.http4s.client.Client",
      "org.http4s.client.blaze._",
      "org.http4s.circe._",
      "org.http4s.Status.Successful",
      "io.circe._",
      "io.circe.generic.semiauto._",
      "shapeless.Coproduct",
      "scala.concurrent.ExecutionContext"
    ).map(PackageName.apply)

    def applyMethod: Printer[(TraitName, ImplName)] =
      (
        (konst(
          "  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): Resource[F, ")) *< string >* konst(
          "[F]] = "),
        konst("BlazeClientBuilder(executionContext).resource.map(") *< string >* konst(".build(_, baseUrl))")
      ).contramapN(identity)

    def openApi[T: Basis[JsonSchemaF, ?]]: Printer[(PackageName, OpenApi[T])] =
      (print.openApiTemplate(applyMethod)).contramap { x =>
        un(imports -> x)
      }
  }

  def traitName[T](openApi: OpenApi[T]): TraitName = s"${normalize(openApi.info.title)}Client"
  def implName[T](openApi: OpenApi[T]): ImplName   = s"${normalize(openApi.info.title)}HttpClient"

  def path: Printer[String] = Printer { s =>
    s"/${s.split("/").filter(_.nonEmpty).map(s => s""" "$s"""").mkString("/")}"
  }

  def body: Printer[String] =
    konst(".withBody(") *< string >* konst(")")

  def methodImpl[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      space *< space *< method[T],
      konst(" = client.expect[Unit](Request[F](method = Method.") *< string,
      konst(", uri = baseUrl ") *< path >* konst("))"),
      optional(body)).contramapN(x => (x, x._1.toUpperCase(), x._2, "updatePet".some)) // FIXME Needs to be implemented

  def impl[T: Basis[JsonSchemaF, ?]](applyMethod: Printer[(TraitName, ImplName)]): Printer[OpenApi[T]] =
    (
      konst("object ") *< string >* konst(" {") *< newLine,
      konst("  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): ") *< string >* konst("[F]"),
      konst(" = new ") *< string >* konst("[F] {") >* newLine,
      space *< space *< space *< space *< packages >* newLine,
      sepBy(methodImpl, "\n") >* newLine *< konst("  }") *< newLine,
      applyMethod >* (newLine *< konst("}"))).contramapN { x =>
      (
        implName(x),
        traitName(x),
        traitName(x),
        List(PackageName(s"${traitName(x)}._")),
        toOperationsWithPath(traitName(x) -> x.paths)._2,
        traitName(x) -> implName(x))
    }

  def openApiTemplate[T: Basis[JsonSchemaF, ?]](
      applyMethod: Printer[(TraitName, ImplName)]): Printer[(List[PackageName], PackageName, OpenApi[T])] =
    (packages >* newLine, impl[T](applyMethod)).contramapN {
      case (imports, packageName, openApi) =>
        (
          imports ++ List(
            s"${packageName.show}.${traitName(openApi)}",
            s"${packageName.show}.models._"
          ).map(PackageName.apply),
          openApi)
    }

}
