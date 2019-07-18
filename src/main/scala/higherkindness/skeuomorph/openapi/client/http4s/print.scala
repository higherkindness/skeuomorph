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
import higherkindness.skeuomorph.Printer.{konst => κ, _}
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi._

import higherkindness.skeuomorph.openapi.print.{schema => _, _}
import higherkindness.skeuomorph.openapi.client.print._
import cats.implicits._
import qq.droste._

object print {
  import schema._
  final case class EntityCodecs[T](name: String, tpe: T)

  def impl[T: Basis[JsonSchemaF, ?]](
      implicit http4sSpecifics: Http4sSpecifics,
      codecs: Printer[Codecs]): Printer[(PackageName, OpenApi[T])] =
    (sepBy(importDef, "\n") >* newLine, implDefinition[T]).contramapN {
      case (packageName, openApi) =>
        (
          packages ++ List(
            s"${packageName.show}.${TraitName(openApi).show}",
            s"${packageName.show}.models._"
          ).map(PackageName.apply),
          openApi)
    }

  val listEnconderPrinter: Printer[Unit] = κ(
    "implicit def listEntityEncoder[T: Encoder]: EntityEncoder[F, List[T]] = jsonEncoderOf[F, List[T]]")
  val listDecoderPrinter: Printer[Unit] = κ(
    "implicit def listEntityDecoder[T: Decoder]: EntityDecoder[F, List[T]] = jsonOf[F, List[T]]")
  val optionListEncoderPrinter: Printer[Unit] = κ(
    "implicit def optionListEntityEncoder[T: Encoder]: EntityEncoder[F, Option[List[T]]] = jsonEncoderOf[F, Option[List[T]]]")
  val optionListDecoderPrinter: Printer[Unit] = κ(
    "implicit def optionListEntityDecoder[T: Decoder]: EntityDecoder[F, Option[List[T]]] = jsonOf[F, Option[List[T]]]")
  type ImplDef[T] = (TraitName, TraitName, List[PackageName], List[OperationWithPath[T]], (TraitName, ImplName))
  def implDefinition[T: Basis[JsonSchemaF, ?]](
      implicit http4sSpecifics: Http4sSpecifics,
      codecs: Printer[Codecs]): Printer[OpenApi[T]] =
    objectDef[ImplDef[T]](
      (
        κ("  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri): ") *< show[TraitName] >* κ("[F]"),
        κ(" = new ") *< show[TraitName] >* κ("[F] {") >* newLine,
        twoSpaces *< twoSpaces *< sepBy(importDef, "\n") >* newLine *<
          twoSpaces *< twoSpaces *< listEnconderPrinter *< newLine *<
          twoSpaces *< twoSpaces *< listDecoderPrinter *< newLine *<
          twoSpaces *< twoSpaces *< optionListEncoderPrinter *< newLine *<
          twoSpaces *< twoSpaces *< optionListDecoderPrinter *< newLine,
        sepBy(twoSpaces *< methodImpl(http4sSpecifics.withBody), "\n") >* newLine *< κ("  }") *< newLine,
        http4sSpecifics.applyMethod).contramapN(identity)).contramap { x =>
      (
        ImplName(x).show,
        List.empty,
        (
          TraitName(x),
          TraitName(x),
          List(PackageName(s"${TraitName(x).show}._")),
          toOperationsWithPath(TraitName(x) -> x.paths)._2,
          TraitName(x) -> ImplName(x)))
    }

  def successResponseImpl[T: Basis[JsonSchemaF, ?]]: Printer[Either[Response[T], Reference]] =
    (κ("case Successful(response) => response.as[") *< responseOrType[T] >* κ("].map(_.asRight)"))
      .contramap(identity)

  def coproduct[A](printer: Printer[A]): Printer[(String, A)] =
    (κ("Coproduct[") *< string >* κ("]"), κ("(") *< printer >* κ(")")).contramapN(identity)

  def coproductIf[A](printer: Printer[A]): Printer[(String, A, Int)] =
    (printer >|< coproduct(printer)).contramap {
      case (x, y, numberOfErrors) if numberOfErrors > 1 => (x -> y).asRight
      case (_, y, _)                                    => y.asLeft
    }

  def statusResponseImpl[T: Basis[JsonSchemaF, ?]](
      implicit codecs: Printer[Codecs]): Printer[(OperationId, String, (Either[Response[T], Reference], Int))] =
    (
      κ("case response if response.status.code == ") *< string >* κ(" => "),
      κ("response.as[") *< string >* κ("]"),
      κ(".map(x => ") *< coproductIf(string >* κ("(x)") >|< κ("x")) >* κ(".asLeft)")).contramapN {
      case (operationId, status, (r, n)) =>
        val (tpe, anonymousType, _) = statusTypesAndSchemas[T](operationId, r)
        val responseTpe             = anonymousType.getOrElse(responseOrType.print(r))
        (
          status,
          responseTpe,
          (
            defaultResponseErrorName(operationId, none),
            anonymousType.fold[Either[String, Unit]](if (tpe === responseTpe) ().asRight else tpe.asLeft)(_ =>
              ().asRight),
            n))
    }

  def defaultResponseImpl[T: Basis[JsonSchemaF, ?]](
      implicit codecs: Printer[Codecs]): Printer[(OperationId, (Either[Response[T], Reference], Int))] =
    (
      κ("case default => default.as[") *< string >* κ("]"),
      κ(".map(x => ") *< coproductIf(string >* κ("(default.status.code, x)")) >* κ(".asLeft)"))
      .contramapN {
        case (operationId, (x, n)) =>
          val (tpe, innerTpe, _) = defaultTypesAndSchemas(operationId, x)
          (innerTpe, (defaultResponseErrorName(operationId, none), tpe, n))
      }

  def responseImpl[T: Basis[JsonSchemaF, ?]](
      implicit codecs: Printer[Codecs]): Printer[(OperationId, String, (Either[Response[T], Reference], Int))] =
    (successResponseImpl[T] >|< defaultResponseImpl[T] >|< statusResponseImpl[T]).contramap {
      case (_, statusCodePattern(code), (x, _)) if successStatusCode(code) => (x.asLeft).asLeft
      case (operationId, "default", x)                                     => (operationId -> x).asRight.asLeft
      case x                                                               => x.asRight
    }

  private def queryParametersFrom[T]: Path.Operation[T] => List[Parameter.Query[T]] =
    _.parameters.collect {
      case Left(x: Parameter.Query[T]) => x
    }

  def requestImpl[T: Basis[JsonSchemaF, ?]](withBody: Printer[String]): Printer[OperationWithPath[T]] =
    (
      κ("Request[F](method = Method.") *< show[HttpVerb],
      κ(", uri = baseUrl ") *< divBy(httpPath[T], unit, sepBy(queryParameter[T], "")) >* κ(")"),
      optional(withBody))
      .contramapN {
        case x @ (verb, path, operation) =>
          val operationId = OperationId(x)
          un(
            second(second(verb -> path)(_ -> queryParametersFrom(operation)))(
              _ -> operation.requestBody.flatMap { requestOrTuple[T](operationId, _) }.map(_.name)))
      }

  def fetchImpl[T: Basis[JsonSchemaF, ?]](withBody: Printer[String])(
      implicit codecs: Printer[Codecs]): Printer[OperationWithPath[T]] =
    (
      κ("fetch[") *< responsesTypes >* κ("]("),
      requestImpl[T](withBody) >* κ(") {") >* newLine,
      sepBy(twoSpaces *< twoSpaces *< twoSpaces *< responseImpl[T], "\n") >* newLine,
      twoSpaces *< twoSpaces *< κ("}"))
      .contramapN {
        case x @ (_, _, operation) =>
          val operationId = OperationId(x)
          (
            (operationId -> operation.responses),
            x,
            operation.responses.toList.map { x =>
              un(operationId -> second(x)(y =>
                y -> operation.responses.filterNot { case (s, _) => successStatusCode(s) }.size))
            },
            ()
          )
      }

  def expectImpl[T: Basis[JsonSchemaF, ?]](withBody: Printer[String]): Printer[OperationWithPath[T]] =
    (κ("expect[") *< responsesTypes >* κ("]("), requestImpl[T](withBody) >* κ(")"))
      .contramapN {
        case x @ (_, _, operation) =>
          val operationId = OperationId(x)
          (operationId -> operation.responses, x)
      }

  def methodImpl[T: Basis[JsonSchemaF, ?]](withBody: Printer[String])(
      implicit codecs: Printer[Codecs]): Printer[OperationWithPath[T]] =
    (
      method[T] >* κ(" = client."),
      expectImpl[T](withBody) >|< fetchImpl[T](withBody)
    ).contramapN {
      case x @ (_, _, operation) =>
        (x, if (operation.responses.size > 1) x.asRight else x.asLeft)
    }

  def queryParameter[T]: Printer[Parameter.Query[T]] =
    (space *< string >* space, κ("(\"") *< divBy(string, κ("\", "), string) >* κ(")")).contramapN { q =>
      def op(x: Parameter.Query[T]): String = if (x.required) "+?" else "+??"
      (op(q), q.name -> q.name)
    }

  def httpPath[T]: Printer[HttpPath] =
    (κ("/ ") *< sepBy(string, " / ")).contramap {
      _.show
        .split("/")
        .filter(_.nonEmpty)
        .map { s =>
          if (s.startsWith("{") && s.endsWith("}"))
            s"${s.tail.init}.show"
          else
            "\"" + s + "\""
        }
        .toList
    }

  private val packages = List(
    "cats.effect._",
    "cats.implicits._",
    "io.circe._",
    "org.http4s._",
    "org.http4s.client.Client",
    "org.http4s.client.blaze._",
    "org.http4s.circe._",
    "org.http4s.Status.Successful",
    "shapeless.Coproduct",
    "scala.concurrent.ExecutionContext"
  ).map(PackageName.apply)

  private val twoSpaces: Printer[Unit] = space *< space

  trait Http4sSpecifics {
    def applyMethod: Printer[(TraitName, ImplName)]
    def withBody: Printer[String]
  }
  object Http4sSpecifics {
    def apply(implicit http4sSpecifics: Http4sSpecifics): Http4sSpecifics = http4sSpecifics
  }
  object v20 {
    implicit val v20Http4sSpecifics: Http4sSpecifics = new Http4sSpecifics {

      def applyMethod: Printer[(TraitName, ImplName)] =
        divBy(
          κ("  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): Resource[F, ") *< show[
            TraitName] >* κ("[F]] ="),
          space,
          κ("BlazeClientBuilder(executionContext).resource.map(") *< show[ImplName] >* κ(".build(_, baseUrl))")
        )

      def withBody: Printer[String] =
        κ(".withEntity(") *< string >* κ(")")
    }
  }
  object v18 {
    implicit val v18Http4sSpecifics: Http4sSpecifics = new Http4sSpecifics {
      def applyMethod: Printer[(TraitName, ImplName)] =
        divBy(
          κ("  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): F[") *< show[
            TraitName] >* κ("[F]] ="),
          space,
          κ("Http1Client[F](config = BlazeClientConfig.defaultConfig.copy(executionContext = executionContext)).map(") *< show[
            ImplName] >* κ(".build(_, baseUrl))")
        )

      def withBody: Printer[String] =
        κ(".withBody(") *< string >* κ(")")
    }
  }
}
