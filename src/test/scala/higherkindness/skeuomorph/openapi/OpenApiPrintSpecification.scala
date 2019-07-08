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

class OpenApiPrintSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  import print._
  import helpers._
  import cats.implicits._
  import OpenApiPrintSpecification._

  "models should able to print" >> {
    implicit def none[T]: Printer[Codecs[T]] = Printer(_ => "")
    "when a basic type is provided" >> {
      model.print(petstoreOpenApi.withSchema("Foo" -> Fixed.string())) must
        ===("""|object models {
               |
               |  type Foo = String
               |  
               |
               |}""".stripMargin)
    }

    "when a object type is provided" >> {
      model.print(petstoreOpenApi.withSchema("Foo" -> obj("bar" -> Fixed.string())())) must ===(
        """|object models {
           |
           |  final case class Foo(bar: Option[String])
           |  
           |
           |}""".stripMargin)
    }

    "when multiple types are provided" >> {
      model.print(
        petstoreOpenApi
          .withSchema("Bar" -> obj("foo" -> Fixed.string())("foo"))
          .withSchema(
            "Bars" -> Fixed.array(Fixed.reference("#/components/schemas/Bar"))
          )) must ===("""|object models {
                         |
                         |  final case class Bar(foo: String)
                         |  type Bars = List[Bar]
                         |  
                         |  
                         |
                         |}""".stripMargin)
    }
  }

  "circe encoders/decoders should able to print" >> {
    import client.http4s.circe._
    val printer = implicitly[Printer[Codecs[JsonSchemaF.Fixed]]]
    "when a object type is provided" >> {

      printer.print(Codecs("Bar", obj("foo" -> Fixed.string())("foo"))) must ===(
        """|object Bar {
           |  import io.circe._
           |  import io.circe.generic.semiauto._
           |  import org.http4s.{EntityEncoder, EntityDecoder}
           |  import org.http4s.circe._
           |  import cats.Applicative
           |  import cats.effect.Sync
           |  implicit val BarEncoder: Encoder[Bar] = deriveEncoder[Bar]
           |  implicit val BarDecoder: Decoder[Bar] = deriveDecoder[Bar]
           |  implicit def BarEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Bar] = jsonEncoderOf[F, Bar]
           |  implicit def OptionBarEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[Bar]] = jsonEncoderOf[F, Option[Bar]]
           |  implicit def BarEntityDecoder[F[_]:Sync]: EntityDecoder[F, Bar] = jsonOf[F, Bar]
           |}""".stripMargin
      )
    }

    "when a array of object is provided" >> {
      printer.print(Codecs("Bars", Fixed.array(Fixed.reference("Bar")))) must ===(
        """|object Bars {
           |  import org.http4s.{EntityEncoder, EntityDecoder}
           |  import org.http4s.circe._
           |  import cats.Applicative
           |  import cats.effect.Sync
           |  implicit def BarsEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Bars] = jsonEncoderOf[F, Bars]
           |  implicit def OptionBarsEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[Bars]] = jsonEncoderOf[F, Option[Bars]]
           |  implicit def BarsEntityDecoder[F[_]:Sync]: EntityDecoder[F, Bars] = jsonOf[F, Bars]
           |}""".stripMargin
      )
    }
  }

  "circe encoders/decoders should able to print empty" >> {
    import client.http4s.circe._
    val printer = implicitly[Printer[Codecs[JsonSchemaF.Fixed]]]
    "when a basic type is provided " >> {
      printer.print(Codecs("Bars", Fixed.string())) must ===(
        """|object Bars {
           |}""".stripMargin
      )
    }
    "when a reference is provided " >> {
      printer.print(Codecs("Bars", Fixed.reference("Foo"))) must ===(
        """|object Bars {
           |}""".stripMargin
      )
    }
  }

  "Client trait should able to print" >> {
    import client.print._
    "when a post operation is provided" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(mediaTypeReferencePost)) must ===( //
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def createPayload(newPayload: NewPayload): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |}""".stripMargin)
    }

    "when a put and delete are provided" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(mediaTypeReferencePutDelete)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayload(id: String): F[Unit]
           |  def updatePayload(id: String, updatePayload: UpdatePayload): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |
           |
           |}""".stripMargin)
    }

    "when get endpoints are provided" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(mediaTypeReferenceGet).withPath(mediaTypeReferenceGetId)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(limit: Option[Int], name: Option[String]): F[Payloads]
           |  def getPayload(id: String): F[Payload]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |
           |
           |}""".stripMargin)
    }

    "when optional body and not optional query parameters is provided" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(mediaTypeOptionBody)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayload(id: String, size: Long, updatePayload: Option[UpdatePayload]): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |}""".stripMargin)
    }

    "when references in the request and the responses" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(references)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(updatePayload: UpdatePayload): F[UpdatedPayload]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |}""".stripMargin)
    }

    "when there are multiple responses with a default one" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponsesWithDefaultOne)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(id: String): F[Either[GetPayloadError, Payload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class GetPayloadUnexpectedErrorResponse(statusCode: Int, value: Error)
           |  type GetPayloadError = GetPayloadUnexpectedErrorResponse
           |
           |}""".stripMargin)
    }

    "when there are multiple responses with not found response" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(notFoundResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(id: String): F[Either[GetPayloadError, Payload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class GetPayloadNotFoundError(value: String)
           |  type GetPayloadError = GetPayloadNotFoundError
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with anonymous objects" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObject)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(id: String): F[Either[UpdatePayloadError, UpdatedPayload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class UpdatedPayload(name: String)
           |  final case class UpdatePayloadNotFound(isDone: Boolean)
           |  type UpdatePayloadError = UpdatePayloadNotFound
           |
           |}""".stripMargin
      )
    }

    "when there are simple response and response with anonymous objects" >> {
      interfaceDefinition.print(anotherPayloadOpenApi.withPath(simpleResponseResponseAnonymousObjects)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait AnotherPayloadClient[F[_]] {
           |  import AnotherPayloadClient._
           |  def updateAnotherPayload(id: String, updateAnotherPayloadRequest: UpdateAnotherPayloadRequest): F[UpdatedPayload]
           |}
           |object AnotherPayloadClient {
           |
           |  final case class UpdateAnotherPayloadRequest(name: String)
           |  final case class UpdatedPayload(name: String)
           |
           |}""".stripMargin
      )
    }

    "when multiple responses with anonymous objects with default response" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObjectAndDefaultOne)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(id: String): F[Either[UpdatePayloadError, UpdatedPayload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class UpdatedPayload(name: String)
           |  final case class UpdatePayloadUnexpectedError(isDone: Boolean)
           |  final case class UpdatePayloadUnexpectedErrorResponse(statusCode: Int, value: UpdatePayloadUnexpectedError)
           |  type UpdatePayloadError = UpdatePayloadUnexpectedErrorResponse
           |
           |}""".stripMargin
      )
    }

    "when multiple responses and multiple error scenarios" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponses)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def createPayload(): F[Either[CreatePayloadError, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class CreatePayloadNotFoundError(value: String)
           |  final case class CreatePayloadUnexpectedErrorResponse(statusCode: Int, value: Error)
           |  type CreatePayloadError = CreatePayloadNotFoundError :+: CreatePayloadUnexpectedErrorResponse :+: CNil
           |
           |}""".stripMargin
      )
    }

    "two operations with default response" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(twoOperationsWithDefaultResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def createPayloads(): F[Either[CreatePayloadsError, Unit]]
           |  def updatePayloads(): F[Either[UpdatePayloadsError, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |
           |  final case class CreatePayloadsUnexpectedError(name: String)
           |  final case class CreatePayloadsUnexpectedErrorResponse(statusCode: Int, value: CreatePayloadsUnexpectedError)
           |  type CreatePayloadsError = CreatePayloadsUnexpectedErrorResponse
           |  final case class UpdatePayloadsUnexpectedError(isDone: Boolean)
           |  final case class UpdatePayloadsUnexpectedErrorResponse(statusCode: Int, value: UpdatePayloadsUnexpectedError)
           |  type UpdatePayloadsError = UpdatePayloadsUnexpectedErrorResponse
           |
           |}""".stripMargin
      )
    }

    "when the failure response is empty" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(emptyErrorResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayloads(): F[Either[DeletePayloadsError, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class DeletePayloadsNotFoundError(value: Unit)
           |  type DeletePayloadsError = DeletePayloadsNotFoundError
           |
           |}""".stripMargin)
    }

    "when multiple failure response are empty" >> {
      interfaceDefinition.print(payloadOpenApi.withPath(multipleEmptyErrorResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayloads(): F[Either[DeletePayloadsError, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class DeletePayloadsNotFoundError(value: Unit)
           |  final case class DeletePayloadsUnexpectedErrorResponse(statusCode: Int, value: Unit)
           |  type DeletePayloadsError = DeletePayloadsNotFoundError :+: DeletePayloadsUnexpectedErrorResponse :+: CNil
           |
           |}""".stripMargin)
    }

    "when a post operation is provided and operation id is not provided" >> {
      interfaceDefinition.print(
        petstoreOpenApi
          .withPath(
            "/pets" -> emptyItemObject.withPost(
              operation[JsonSchemaF.Fixed](
                request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPet"))),
                responses = "200"          -> response("Null response")
              )
            ))
          .withPath(
            "/pets/{id}" -> emptyItemObject.withPut(
              operation[JsonSchemaF.Fixed](
                request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePet"))),
                responses = "201"          -> response("Null response")
              ).withParameter(path("id", Fixed.string()))
            ))
          .withPath(
            "/pets/{id}/owners/" -> emptyItemObject.withGet(
              operation[JsonSchemaF.Fixed](
                request(),
                responses = "201" -> response(
                  "Null response",
                  "application/json" -> mediaType(Fixed.reference("#/components/schemas/Owners")))
              ).withParameter(path("id", Fixed.string()))
            ))
      ) must ===("""|import models._
                    |import shapeless.{:+:, CNil}
                    |trait PetstoreClient[F[_]] {
                    |  import PetstoreClient._
                    |  def createPets(newPet: NewPet): F[Unit]
                    |  def updatePets(id: String, updatePet: UpdatePet): F[Unit]
                    |  def getOwnersPets(id: String): F[Owners]
                    |}
                    |object PetstoreClient {
                    |
                    |
                    |
                    |
                    |
                    |
                    |
                    |
                    |}""".stripMargin)
    }

  }

  "share http4s impl should able to print" >> {
    import client.print._
    import client.http4s.print.implDefinition
    import client.http4s.print.Http4sSpecifics
    implicit val none: Http4sSpecifics = new Http4sSpecifics {
      def none[A]                                     = Printer.unit.contramap[A](_ => ())
      def applyMethod: Printer[(TraitName, ImplName)] = none
      def withBody: Printer[String]                   = none

    }

    "when a put and delete are provided" >> {
      implDefinition.print(petstoreOpenApi.withPath(mediaTypeReferencePutDelete)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def deletePayload(id: String): F[Unit] = client.expect[Unit](Request[F](method = Method.DELETE, uri = baseUrl / "payloads" / id.show))
           |    def updatePayload(id: String, updatePayload: UpdatePayload): F[Unit] = client.expect[Unit](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show))
           |  }
           |
           |}""".stripMargin)
    }

    "when get endpoints are provided" >> {
      implDefinition.print(petstoreOpenApi.withPath(mediaTypeReferenceGet).withPath(mediaTypeReferenceGetId)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def getPayload(limit: Option[Int], name: Option[String]): F[Payloads] = client.expect[Payloads](Request[F](method = Method.GET, uri = baseUrl / "payloads" +?? ("limit", limit) +?? ("name", name)))
           |    def getPayload(id: String): F[Payload] = client.expect[Payload](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when optional body and not optional query parameters is provided" >> {
      implDefinition.print(petstoreOpenApi.withPath(mediaTypeOptionBody)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def deletePayload(id: String, size: Long, updatePayload: Option[UpdatePayload]): F[Unit] = client.expect[Unit](Request[F](method = Method.DELETE, uri = baseUrl / "payloads" / id.show +? ("size", size)))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when references in the request and the responses" >> {
      implDefinition.print(petstoreOpenApi.withPath(references)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def updatePayload(updatePayload: UpdatePayload): F[UpdatedPayload] = client.expect[UpdatedPayload](Request[F](method = Method.PUT, uri = baseUrl / "payloads"))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with a default one" >> {
      implDefinition.print(petstoreOpenApi.withPath(multipleResponsesWithDefaultOne)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def getPayload(id: String): F[Either[GetPayloadError, Payload]] = client.fetch[Either[GetPayloadError, Payload]](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[Payload].map(_.asRight)
           |      case default => default.as[Error].map(x => GetPayloadUnexpectedErrorResponse(default.status.code, x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin)
    }

    "when there are multiple responses with not found response" >> {
      implDefinition.print(petstoreOpenApi.withPath(notFoundResponse)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def getPayload(id: String): F[Either[GetPayloadError, Payload]] = client.fetch[Either[GetPayloadError, Payload]](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[Payload].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[String].map(x => GetPayloadNotFoundError(x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin)
    }

    "when there are simple response and response with anonymous objects" >> {
      implDefinition.print(petstoreOpenApi.withPath(simpleResponseResponseAnonymousObjects)) must ===(
        """|object PetstoreHttpClient {
          |
          |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
          |    import PetstoreClient._
          |    def updateAnotherPayload(id: String, updateAnotherPayloadRequest: UpdateAnotherPayloadRequest): F[UpdatedPayload] = client.expect[UpdatedPayload](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show))
          |  }
          |
          |}""".stripMargin)
    }

    "when multiple responses with anonymous objects with default response" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObjectAndDefaultOne)) must ===(
        """|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |    def updatePayload(id: String): F[Either[UpdatePayloadError, UpdatedPayload]] = client.fetch[Either[UpdatePayloadError, UpdatedPayload]](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[UpdatedPayload].map(_.asRight)
           |      case default => default.as[UpdatePayloadUnexpectedError].map(x => UpdatePayloadUnexpectedErrorResponse(default.status.code, x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin)
    }

    "when multiple responses and multiple error scenarios" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleResponses)) must ===(
        """|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |    def createPayload(): F[Either[CreatePayloadError, Unit]] = client.fetch[Either[CreatePayloadError, Unit]](Request[F](method = Method.POST, uri = baseUrl / "payloads")) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[String].map(x => Coproduct[CreatePayloadError](CreatePayloadNotFoundError(x)).asLeft)
           |      case default => default.as[Error].map(x => Coproduct[CreatePayloadError](CreatePayloadUnexpectedErrorResponse(default.status.code, x)).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with anonymous objects" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObject)) must ===(
        """|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |    def updatePayload(id: String): F[Either[UpdatePayloadError, UpdatedPayload]] = client.fetch[Either[UpdatePayloadError, UpdatedPayload]](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[UpdatedPayload].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[UpdatePayloadNotFound].map(x => x.asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when the failure response is empty" >> {
      implDefinition.print(payloadOpenApi.withPath(emptyErrorResponse)) must ===(
        """|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |    def deletePayloads(): F[Either[DeletePayloadsError, Unit]] = client.fetch[Either[DeletePayloadsError, Unit]](Request[F](method = Method.DELETE, uri = baseUrl / "payloads")) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[Unit].map(x => DeletePayloadsNotFoundError(x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when multiple failure response are empty" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleEmptyErrorResponse)) must ===(
        """|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |    def deletePayloads(): F[Either[DeletePayloadsError, Unit]] = client.fetch[Either[DeletePayloadsError, Unit]](Request[F](method = Method.DELETE, uri = baseUrl / "payloads")) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[Unit].map(x => Coproduct[DeletePayloadsError](DeletePayloadsNotFoundError(x)).asLeft)
           |      case default => default.as[Unit].map(x => Coproduct[DeletePayloadsError](DeletePayloadsUnexpectedErrorResponse(default.status.code, x)).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }
  }

  "http4s 0.20.x should able to print" >> {
    import client.http4s.print.impl
    import client.http4s.print.v20._

    "when a post operation is provided" >> {
      impl.print(
        PackageName("petstore") -> petstoreOpenApi
          .withPath(mediaTypeReferences)
          .withSchema("NewPayloads" -> Fixed.array(Fixed.reference("NewPayload")))) must ===(
        """|import cats.effect._
           |import cats.implicits._
           |import org.http4s._
           |import org.http4s.client.Client
           |import org.http4s.client.blaze._
           |import org.http4s.circe._
           |import org.http4s.Status.Successful
           |import shapeless.Coproduct
           |import scala.concurrent.ExecutionContext
           |import petstore.PetstoreClient
           |import petstore.models._
           |import NewPayloads._
           |object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def createPayloads(newPayloads: NewPayloads): F[Either[CreatePayloadsError, Unit]] = client.fetch[Either[CreatePayloadsError, Unit]](Request[F](method = Method.POST, uri = baseUrl / "payloads").withEntity(newPayloads)) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case default => default.as[Error].map(x => CreatePayloadsUnexpectedErrorResponse(default.status.code, x).asLeft)
           |    }
           |    def updatePayloads(payloads: Payloads): F[Payloads] = client.expect[Payloads](Request[F](method = Method.PUT, uri = baseUrl / "payloads").withEntity(payloads))
           |  }
           |  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): Resource[F, PetstoreClient[F]] = BlazeClientBuilder(executionContext).resource.map(PetstoreHttpClient.build(_, baseUrl))
           |}""".stripMargin
      )
    }
  }

  "http4s 0.18.x should able to print" >> {
    import client.http4s.print.impl
    import client.http4s.print.v18._

    "when a post operation is provided" >> {
      impl.print(PackageName("petstore") -> petstoreOpenApi.withPath(mediaTypeReferences)) must ===(
        """|import cats.effect._
           |import cats.implicits._
           |import org.http4s._
           |import org.http4s.client.Client
           |import org.http4s.client.blaze._
           |import org.http4s.circe._
           |import org.http4s.Status.Successful
           |import shapeless.Coproduct
           |import scala.concurrent.ExecutionContext
           |import petstore.PetstoreClient
           |import petstore.models._
           |object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    def createPayloads(newPayloads: NewPayloads): F[Either[CreatePayloadsError, Unit]] = client.fetch[Either[CreatePayloadsError, Unit]](Request[F](method = Method.POST, uri = baseUrl / "payloads").withBody(newPayloads)) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case default => default.as[Error].map(x => CreatePayloadsUnexpectedErrorResponse(default.status.code, x).asLeft)
           |    }
           |    def updatePayloads(payloads: Payloads): F[Payloads] = client.expect[Payloads](Request[F](method = Method.PUT, uri = baseUrl / "payloads").withBody(payloads))
           |  }
           |  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): F[PetstoreClient[F]] = Http1Client[F](config = BlazeClientConfig.defaultConfig.copy(executionContext = executionContext)).map(PetstoreHttpClient.build(_, baseUrl))
           |}""".stripMargin
      )
    }
  }
}

object OpenApiPrintSpecification {
  import JsonSchemaF.Fixed
  import helpers._

  private val pathId        = path("id", Fixed.string())
  private val payloadPath   = "/payloads"
  private val payloadPathId = s"$payloadPath/{id}"

  def petstoreOpenApi[T]       = openApi[T]("Petstore")
  def payloadOpenApi[T]        = openApi[T]("Payload")
  def anotherPayloadOpenApi[T] = openApi[T]("AnotherPayload")

  private val successPayload = "200" -> response(
    "Null response",
    "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payload"))
  )

  private val successNull = "200" -> response[JsonSchemaF.Fixed]("Null response")

  private val defaultError = "default" -> response(
    "Unexpected error",
    "application/json" -> mediaType(Fixed.reference("#/components/schemas/Error"))
  )

  private val notFound = "404" -> response(
    "Not found",
    "application/json" -> mediaType(Fixed.string())
  )

  val mediaTypeReferencePost = "/payloads" -> emptyItemObject.withPost(
    operation[JsonSchemaF.Fixed](
      request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPayload"))),
      responses = "201"          -> response("Null response")
    ).withOperationId("createPayload")
  )

  val mediaTypeReferences = "/payloads" -> emptyItemObject
    .withPost(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPayloads"))),
        responses = "201"          -> response(""),
        defaultError
      ).withOperationId("createPayloads")
    )
    .withPut(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/Payloads"))),
        responses = "200" -> response(
          "",
          "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payloads")))
      ).withOperationId("updatePayloads")
    )

  val mediaTypeReferencePutDelete = payloadPathId -> emptyItemObject
    .withPut(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePayload"))),
        successNull
      ).withOperationId("updatePayload").withParameter(pathId))
    .withDelete(
      operation[JsonSchemaF.Fixed](
        request(),
        successNull
      ).withOperationId("deletePayload").withParameter(pathId))

  val mediaTypeReferenceGet = payloadPath -> emptyItemObject.withGet(
    operationWithResponses[JsonSchemaF.Fixed](
      responses = "200" -> response(
        "",
        "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payloads")))
    ).withOperationId("getPayload")
      .withParameter(query("limit", Fixed.integer()))
      .withParameter(query("name", Fixed.string()))
  )
  val mediaTypeReferenceGetId = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](successPayload)
        .withOperationId("getPayload")
        .withParameter(path("id", Fixed.string())))

  val mediaTypeOptionBody = payloadPathId -> emptyItemObject
    .withDelete(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePayload"))).optional,
        successNull
      ).withOperationId("deletePayload")
        .withParameter(path("id", Fixed.string()))
        .withParameter(query("size", Fixed.long(), required = true)))

  val references = payloadPath -> emptyItemObject
    .withPut(
      operationWithReferences[JsonSchemaF.Fixed](
        reference("#/components/schemas/UpdatePayload"),
        responses = "200" -> reference("#/components/schemas/UpdatedPayload")
      ).withOperationId("updatePayload"))

  val multipleResponsesWithDefaultOne = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successPayload,
        defaultError
      ).withOperationId("getPayload").withParameter(path("id", Fixed.string()))
    )

  val notFoundResponse = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successPayload,
        notFound
      ).withOperationId("getPayload").withParameter(path("id", Fixed.string())))

  val multipleResponsesWithAnonymousObject = payloadPathId -> emptyItemObject
    .withPut(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = "200" -> response(
          "Updated payload",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        ),
        "404" -> response(
          "Not found",
          "application/json" -> mediaType(obj("isDone" -> Fixed.boolean())("isDone"))
        )
      ).withOperationId("updatePayload").withParameter(path("id", Fixed.string()))
    )

  val simpleResponseResponseAnonymousObjects = "/payloads/{id}" -> emptyItemObject
    .withPut(
      operation[JsonSchemaF.Fixed](
        request(
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        ),
        responses = "200" -> response(
          "Updated payload",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        )
      ).withOperationId("updateAnotherPayload").withParameter(path("id", Fixed.string()))
    )

  val multipleResponsesWithAnonymousObjectAndDefaultOne = "/payloads/{id}" -> emptyItemObject
    .withPut(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = "200" -> response(
          "Updated payload",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        ),
        "default" -> response(
          "Unexpected error",
          "application/json" -> mediaType(obj("isDone" -> Fixed.boolean())("isDone"))
        )
      ).withOperationId("updatePayload").withParameter(path("id", Fixed.string()))
    )

  val multipleResponses = "/payloads" -> emptyItemObject
    .withPost(
      operationWithResponses[JsonSchemaF.Fixed](successNull, notFound, defaultError).withOperationId("createPayload")
    )

  val twoOperationsWithDefaultResponse = "/payloads/" -> emptyItemObject
    .withPost(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successNull,
        "default" -> response(
          "Unexpected error",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        )
      )
    )
    .withPut(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successNull,
        "default" -> response(
          "Unexpected error",
          "application/json" -> mediaType(obj("isDone" -> Fixed.boolean())("isDone"))
        )
      )
    )

  val emptyErrorResponse = "/payloads/" -> emptyItemObject.withDelete(
    operationWithResponses[JsonSchemaF.Fixed](
      responses = successNull,
      "404" -> response("Not found")
    )
  )

  val multipleEmptyErrorResponse = "payloads" -> emptyItemObject.withDelete(
    operationWithResponses[JsonSchemaF.Fixed](
      responses = successNull,
      "404"     -> response("Not found"),
      "default" -> response("Unexpected error")
    )
  )
}
