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
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi._
import higherkindness.skeuomorph.openapi.client.print._
import cats.implicits._
import qq.droste._

object print {
  import schema._

  def openApi[T: Basis[JsonSchemaF, ?]](implicit http4sSpecifics: Http4sSpecifics): Printer[(PackageName, OpenApi[T])] =
    (imports >* newLine, impl[T]).contramapN {
      case (packageName, openApi) =>
        (
          packages ++ List(
            s"${packageName.show}.${TraitName(openApi).show}",
            s"${packageName.show}.models._"
          ).map(PackageName.apply),
          openApi)
    }

  def impl[T: Basis[JsonSchemaF, ?]](implicit http4sSpecifics: Http4sSpecifics): Printer[OpenApi[T]] =
    (
      konst("object ") *< show[ImplName] >* konst(" {") *< newLine,
      sepBy(twoSpaces *< (circeEncoder >|< circeDecoder), "\n") >* newLine,
      konst("  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): ") *< show[TraitName] >* konst("[F]"),
      konst(" = new ") *< show[TraitName] >* konst("[F] {") >* newLine,
      twoSpaces *< twoSpaces *< imports >* newLine,
      sepBy(twoSpaces *< twoSpaces *< (entityEncoder >|< entityDecoder), "\n") >* newLine,
      sepBy(twoSpaces *< methodImpl(http4sSpecifics.withBody), "\n") >* newLine *< konst("  }") *< newLine,
      http4sSpecifics.applyMethod >* (newLine *< konst("}"))).contramapN { x =>
      val encoderAndDecoders = encoderAndDecodersFrom(x)
      (
        ImplName(x),
        encoderAndDecoders,
        TraitName(x),
        TraitName(x),
        List(PackageName(s"${TraitName(x).show}._")),
        encoderAndDecoders,
        toOperationsWithPath(TraitName(x) -> x.paths)._2,
        TraitName(x) -> ImplName(x))
    }

  def encoderAndDecodersFrom[T: Basis[JsonSchemaF, ?]](openApi: OpenApi[T]): List[Either[Encoder[T], Decoder[T]]] = {
    val xs = openApi.components.toList.flatMap(_.schemas.map(_._1))
    xs.flatMap { x =>
      List(
        Encoder[T](Var(x, Tpe.apply(x))).asLeft,
        Encoder[T](Var(s"Option$x", Tpe.apply[T](x).copy(required = false))).asLeft)
    } ++
      xs.map(x => Decoder[T](Var(x, Tpe.apply(x))).asRight)
  }

  def varTuple[T](x: Var[T]): (String, Tpe[T], Tpe[T]) = ((x.name, x.tpe, x.tpe))

  def circeDecoder[T: Basis[JsonSchemaF, ?]]: Printer[Decoder[T]] =
    (
      konst("implicit val ") *< string >* konst("Decoder: "),
      konst("Decoder[") *< tpe[T] >* konst("] = "),
      konst("deriveDecoder[") *< tpe[T] >* konst("]"))
      .contramapN(x => varTuple(x.value))

  def circeEncoder[T: Basis[JsonSchemaF, ?]]: Printer[Encoder[T]] =
    (
      konst("implicit val ") *< string >* konst("Encoder: "),
      konst("Encoder[") *< tpe[T] >* konst("] = "),
      konst("deriveEncoder[") *< tpe[T] >* konst("]"))
      .contramapN(x => varTuple(x.value))

  def entityDecoder[T: Basis[JsonSchemaF, ?]]: Printer[Decoder[T]] =
    (
      konst("implicit val ") *< string >* konst("EntityDecoder: "),
      konst("EntityDecoder[F, ") *< tpe[T] >* konst("] = "),
      konst("jsonOf[F, ") *< tpe[T] >* konst("]"))
      .contramapN(x => varTuple(x.value))

  def entityEncoder[T: Basis[JsonSchemaF, ?]]: Printer[Encoder[T]] =
    (
      konst("implicit val ") *< string >* konst("EntityEncoder: "),
      konst("EntityEncoder[F, ") *< tpe[T] >* konst("] = "),
      konst("jsonEncoderOf[F, ") *< tpe[T] >* konst("]"))
      .contramapN(x => varTuple(x.value))

  def successResponseImpl[T: Basis[JsonSchemaF, ?]]: Printer[Either[Response[T], Reference]] =
    (konst("case Successful(response) => response.as[") *< responseOrType[T] >* konst("].map(_.asRight)"))
      .contramap(identity)

  def coproduct[A](printer: Printer[A]): Printer[(String, A)] =
    (konst("Coproduct[") *< string >* konst("]"), konst("(") *< printer >* konst(")")).contramapN(identity)

  def coproductIf[A](printer: Printer[A]): Printer[(String, A, Int)] =
    (printer >|< coproduct(printer)).contramap {
      case (x, y, numberOfErrors) if numberOfErrors > 1 => (x -> y).asRight
      case (_, y, _)                                    => y.asLeft
    }

  def statusResponseImpl[T: Basis[JsonSchemaF, ?]]: Printer[(String, (Either[Response[T], Reference], Int))] =
    (
      konst("case response if response.status.code == ") *< string >* konst(" => "),
      konst("response.as[") *< responseOrType[T] >* konst("]"),
      konst(".map(x => ") *< coproductIf(string >* konst("(x)")) >* konst(".asLeft)")).contramapN {
      case (status, (tpe, n)) =>
        val (innerTpe, _) = typesAndSchemas[T](status, tpe)
        (status, tpe, (innerTpe, innerTpe, n))
    }

  def defaultResponseImpl[T: Basis[JsonSchemaF, ?]]: Printer[(Either[Response[T], Reference], Int)] =
    (
      konst("case default => default.as[") *< responseOrType[T] >* konst("]"),
      konst(".map(x => ") *< coproductIf(konst("UnexpectedErrorResponse(default.status.code, x)")) >* konst(".asLeft)"))
      .contramapN { x =>
        second(x)(y => ("UnexpectedErrorResponse", (), y))
      }

  def responseImpl[T: Basis[JsonSchemaF, ?]]: Printer[(String, (Either[Response[T], Reference], Int))] =
    (successResponseImpl[T] >|< statusResponseImpl[T] >|< defaultResponseImpl[T]).contramap {
      case (statusCodePattern(code), x) if successStatusCode(code) => (x._1.asLeft).asLeft
      case ("default", x)                                          => x.asRight
      case (s, x)                                                  => (s -> x).asRight.asLeft
    }

  def requestImpl[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      konst("Request[F](method = Method.") *< show[HttpVerb],
      konst(", uri = baseUrl ") *< divBy(httpPath[T], sepBy(queryParameter[T], ""))(unit) >* konst(")"))
      .contramapN(x =>
        (x._1, (x._2, x._3.parameters.flatMap(_.left.toOption).collect {
          case x: Parameter.Query[T] => x
        })))

  def fetchImpl[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      konst("fetch[") *< responsesTypes >* konst("]("),
      requestImpl[T] >* konst(") {") >* newLine,
      sepBy(twoSpaces *< twoSpaces *< twoSpaces *< responseImpl[T], "\n") >* newLine,
      twoSpaces *< twoSpaces *< konst("}"))
      .contramapN { x =>
        val operationId       = OperationId(x)
        val (_, _, operation) = x
        (
          (operationId -> operation.responses),
          x,
          operation.responses.toList.map(second(_)(y =>
            y -> operation.responses.filterNot(x => successStatusCode(x._1)).size)),
          ())
      }

  def expectImpl[T: Basis[JsonSchemaF, ?]](withBody: Printer[String]): Printer[OperationWithPath[T]] =
    (konst("expect[") *< responsesTypes >* konst("]("), requestImpl[T] >* konst(")"), optional(withBody))
      .contramapN { x =>
        val operationId = OperationId(x)
        (operationId -> x._3.responses, x, x._3.requestBody.flatMap { requestOrTuple[T](operationId, _) }.map(_.name))
      }

  def methodImpl[T: Basis[JsonSchemaF, ?]](withBody: Printer[String]): Printer[OperationWithPath[T]] =
    (
      method[T] >* konst(" = client."),
      expectImpl[T](withBody) >|< fetchImpl[T]
    ).contramapN { x =>
      (x, if (x._3.responses.size > 1) x.asRight else x.asLeft)
    }

  def queryParameter[T]: Printer[Parameter.Query[T]] =
    (space *< string >* space, konst("(\"") *< divBy(string, string)(konst("\", ")) >* konst(")")).contramapN { q =>
      def op(x: Parameter.Query[T]): String = if (x.required) "+?" else "+??"
      (op(q), q.name -> q.name)
    }

  def httpPath[T]: Printer[HttpPath] =
    (konst("/ ") *< sepBy(string, " / ")).contramap {
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
          konst(
            "  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): Resource[F, ") *< show[
            TraitName] >* konst("[F]] ="),
          konst("BlazeClientBuilder(executionContext).resource.map(") *< show[ImplName] >* konst(".build(_, baseUrl))")
        )(space)

      def withBody: Printer[String] =
        konst(".withBody(") *< string >* konst(")")
    }
  }
  object v18 {
    implicit val v18Http4sSpecifics: Http4sSpecifics = new Http4sSpecifics {
      def applyMethod: Printer[(TraitName, ImplName)] =
        divBy(
          konst("  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): F[") *< show[
            TraitName] >* konst("[F]] ="),
          konst(
            "Http1Client[F](config = BlazeClientConfig.defaultConfig.copy(executionContext = executionContext)).map(") *< show[
            ImplName] >* konst(".build(_, baseUrl))")
        )(space)

      def withBody: Printer[String] =
        konst(".withEntity(") *< string >* konst(")")
    }
  }
}
