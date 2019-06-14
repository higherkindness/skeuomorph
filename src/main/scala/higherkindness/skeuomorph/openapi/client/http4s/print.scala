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

    val packages = List(
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
          "  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): Resource[F, ")) *< show[
          TraitName] >* konst("[F]] = "),
        konst("BlazeClientBuilder(executionContext).resource.map(") *< show[ImplName] >* konst(".build(_, baseUrl))")
      ).contramapN(identity)

    def openApi[T: Basis[JsonSchemaF, ?]]: Printer[(PackageName, OpenApi[T])] =
      (print.openApiTemplate(applyMethod)).contramap { x =>
        un(packages -> x)
      }
  }

  def queryParameters[T]: Printer[List[Parameter.Query[T]]] = Printer { q =>
    def op(x: Parameter.Query[T]): String = if (x.required) "+?" else "+??"
    q.map(x => s""" ${op(x)} ("${x.name}", ${x.name})""").mkString
  }
  def httpPath[T]: Printer[HttpPath] = Printer {
    case s =>
      s"/ ${s.show
        .split("/")
        .filter(_.nonEmpty)
        .map { s =>
          if (s.startsWith("{") && s.endsWith("}"))
            s"${s.tail.init}.show"
          else
            s""""$s""""
        }
        .mkString(" / ")}"
  }

  def body: Printer[String] =
    konst(".withBody(") *< string >* konst(")")

  def methodImpl[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      space *< space *< method[T],
      konst(" = client.expect[") *< responsesTypes >* konst("]"),
      konst("(Request[F](method = Method.") *< show[HttpVerb],
      konst(", uri = baseUrl ") *< pair(httpPath[T], queryParameters[T]) >* konst("))"),
      optional(body)).contramapN { x =>
      val operationId = OperationId(x)
      (x, operationId -> x._3.responses, x._1, (x._2, x._3.parameters.flatMap(_.left.toOption).collect {
        case x: Parameter.Query[T] => x
      }), x._3.requestBody.flatMap { requestOrTuple[T](operationId, _) }.map(_.name))
    }

  def impl[T: Basis[JsonSchemaF, ?]](applyMethod: Printer[(TraitName, ImplName)]): Printer[OpenApi[T]] =
    (
      konst("object ") *< show[ImplName] >* konst(" {") *< newLine,
      konst("  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): ") *< show[TraitName] >* konst("[F]"),
      konst(" = new ") *< show[TraitName] >* konst("[F] {") >* newLine,
      space *< space *< space *< space *< imports >* newLine,
      sepBy(methodImpl, "\n") >* newLine *< konst("  }") *< newLine,
      applyMethod >* (newLine *< konst("}"))).contramapN { x =>
      (
        ImplName(x),
        TraitName(x),
        TraitName(x),
        List(PackageName(s"${TraitName(x).show}._")),
        toOperationsWithPath(TraitName(x) -> x.paths)._2,
        TraitName(x) -> ImplName(x))
    }

  def openApiTemplate[T: Basis[JsonSchemaF, ?]](
      applyMethod: Printer[(TraitName, ImplName)]): Printer[(List[PackageName], PackageName, OpenApi[T])] =
    (imports >* newLine, impl[T](applyMethod)).contramapN {
      case (imports, packageName, openApi) =>
        (
          imports ++ List(
            s"${packageName.show}.${TraitName(openApi).show}",
            s"${packageName.show}.models._"
          ).map(PackageName.apply),
          openApi)
    }

}
