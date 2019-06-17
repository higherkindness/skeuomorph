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

  def encoderAndDecodersFrom[T: Basis[JsonSchemaF, ?]](openApi: OpenApi[T]): List[Either[EncoderName, DecoderName]] = {
    val xs = openApi.components.toList.flatMap(_.schemas.map(_._1))

    xs.map(EncoderName.apply).map(_.asLeft) ++ xs.map(DecoderName.apply).map(_.asRight)
  }

  def circeDecoder[T: Basis[JsonSchemaF, ?]]: Printer[DecoderName] =
    (
      konst("implicit val ") *< show[DecoderName] >* konst("Decoder: "),
      konst("Decoder[") *< show[DecoderName] >* konst("] = "),
      konst("deriveDecoder[") *< show[DecoderName] >* konst("]"))
      .contramapN(x => (x, x, x))

  def circeEncoder[T: Basis[JsonSchemaF, ?]]: Printer[EncoderName] =
    (
      konst("implicit val ") *< show[EncoderName] >* konst("Encoder: "),
      konst("Encoder[") *< show[EncoderName] >* konst("] = "),
      konst("deriveEncoder[") *< show[EncoderName] >* konst("]"))
      .contramapN(x => (x, x, x))

  def entityDecoder[T: Basis[JsonSchemaF, ?]]: Printer[DecoderName] =
    (
      konst("implicit val ") *< show[DecoderName] >* konst("EntityDecoder: "),
      konst("EntityDecoder[F, ") *< show[DecoderName] >* konst("] = "),
      konst("jsonOf[F, ") *< show[DecoderName] >* konst("]"))
      .contramapN(x => (x, x, x)) //

  def entityEncoder[T: Basis[JsonSchemaF, ?]]: Printer[EncoderName] =
    (
      konst("implicit val ") *< show[EncoderName] >* konst("EntityEncoder: "),
      konst("EntityEncoder[F, ") *< show[EncoderName] >* konst("] = "),
      konst("jsonEncoderOf[F, ") *< show[EncoderName] >* konst("]"))
      .contramapN(x => (x, x, x)) //

  def methodImpl[T: Basis[JsonSchemaF, ?]](withBody: Printer[String]): Printer[OperationWithPath[T]] =
    (
      method[T],
      konst(" = client.expect[") *< responsesTypes >* konst("]"),
      konst("(Request[F](method = Method.") *< show[HttpVerb],
      konst(", uri = baseUrl ") *< divBy(httpPath[T], sepBy(queryParameter[T], ""))(unit) >* konst("))"),
      optional(withBody)).contramapN { x =>
      val operationId = OperationId(x)
      (x, operationId -> x._3.responses, x._1, (x._2, x._3.parameters.flatMap(_.left.toOption).collect {
        case x: Parameter.Query[T] => x
      }), x._3.requestBody.flatMap { requestOrTuple[T](operationId, _) }.map(_.name))
    }

  def queryParameter[T]: Printer[Parameter.Query[T]] =
    (space *< string >* space, konst("""("""") *< divBy(string, string)(konst("""", """)) >* konst(")")).contramapN {
      q =>
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
            s""""$s""""
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
