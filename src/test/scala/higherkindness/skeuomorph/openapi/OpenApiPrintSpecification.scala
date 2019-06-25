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
    "when a basic type is provided" >> {
      model.print(components("Foo" -> Fixed.string())) must
        ===("""|object models {
             |  type Foo = String
             |}""".stripMargin)
    }

    "when a object type is provided" >> {
      model.print(components("Foo" -> obj("bar" -> Fixed.string())())) must ===(
        """|object models {
           |  final case class Foo(bar: Option[String])
           |}""".stripMargin)
    }

    "when multiple types are provided" >> {
      model.print(
        components(
          "Bar"  -> obj("foo" -> Fixed.string())("foo"),
          "Bars" -> Fixed.array(Fixed.reference("#/components/schemas/Bar"))
        )) must ===("""|object models {
                       |  final case class Bar(foo: String)
                       |  type Bars = List[Bar]
                       |}""".stripMargin)
    }
  }

  "Client trait should able to print" >> {
    import client.print._
    "when a post operation is provided" >> {
      operations.print(paths()(mediaTypeReferencePost)) must
        ===("""|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def createPayload(newPayload: NewPayload): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |}""".stripMargin)
    }

    "when a put and delete are provided" >> {
      operations.print(paths()(mediaTypeReferencePutDelete)) must ===(
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
           |}""".stripMargin)
    }

    "when get endpoints are provided" >> {
      operations.print(paths()(mediaTypeReferenceGet, mediaTypeReferenceGetId)) must ===(
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
           |}""".stripMargin)
    }

    "when optional body and not optional query parameters is provided" >> {
      operations.print(paths()(mediaTypeOptionBody)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayload(id: String, size: Long, updatePayload: Option[UpdatePayload]): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |}""".stripMargin)
    }

    "when references in the request and the responses" >> {
      operations.print(paths()(references)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(updatePayload: UpdatePayload): F[UpdatedPayload]
           |}
           |object PayloadClient {
           |
           |
           |}""".stripMargin)
    }

    "when there are multiple responses with a default one" >> {
      operations.print(paths()(multipleResponsesWithDefaultOne)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(id: String): F[Either[GetPayloadError, Payload]]
           |}
           |object PayloadClient {
           |
           |  final case class UnexpectedErrorResponse(statusCode: Int, value: Error)
           |  type GetPayloadError = UnexpectedErrorResponse
           |}""".stripMargin)
    }

    "when there are multiple responses with not found response" >> {
      operations.print(
        paths()(notFoundResponse)
      ) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(id: String): F[Either[GetPayloadError, Payload]]
           |}
           |object PayloadClient {
           |
           |  final case class NotFoundError(value: String)
           |  type GetPayloadError = NotFoundError
           |}""".stripMargin
      )
    }

    "when there are multiple responses with anonymous objects" >> {
      operations.print(
        paths()(multipleResponsesWithAnonymousObject)
      ) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(id: String): F[Either[UpdatePayloadError, UpdatedPayload]]
           |}
           |object PayloadClient {
           |
           |  final case class UpdatedPayload(name: String)
           |  final case class NotFound(isDone: Boolean)
           |  type UpdatePayloadError = NotFound
           |}""".stripMargin
      )
    }

    "when there are simple response and response with anonymous objects" >> {
      operations.print(paths("AnotherPayloadClient")(simpleResponseResponseAnonymousObjects)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait AnotherPayloadClient[F[_]] {
           |  import AnotherPayloadClient._
           |  def updateAnotherPayload(id: String, updateAnotherPayloadRequest: UpdateAnotherPayloadRequest): F[UpdatedPayload]
           |}
           |object AnotherPayloadClient {
           |  final case class UpdateAnotherPayloadRequest(name: String)
           |  final case class UpdatedPayload(name: String)
           |}""".stripMargin
      )
    }

    "when  multiple responses with anonymous objects with default response" >> {
      operations.print(paths()(multipleResponsesWithAnonymousObjectAndDefaultOne)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(id: String): F[Either[UpdatePayloadError, UpdatedPayload]]
           |}
           |object PayloadClient {
           |
           |  final case class UpdatedPayload(name: String)
           |  final case class UnexpectedError(isDone: Boolean)
           |  final case class UnexpectedErrorResponse(statusCode: Int, value: UnexpectedError)
           |  type UpdatePayloadError = UnexpectedErrorResponse
           |}""".stripMargin
      )
    }

    "when a post operation is provided and operation id is not provided" >> {
      operations.print(
        paths("PetsClient")(
          "/pets" -> emptyItemObject.withPost(
            operation[JsonSchemaF.Fixed](
              request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPet"))),
              responses = "200"          -> response("Null response")
            )
          ),
          "/pets/{id}" -> emptyItemObject.withPut(
            operation[JsonSchemaF.Fixed](
              request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePet"))),
              responses = "201"          -> response("Null response")
            ).withParameter(path("id", Fixed.string()))
          ),
          "/pets/{id}/owners/" -> emptyItemObject.withGet(
            operation[JsonSchemaF.Fixed](
              request(),
              responses = "201" -> response(
                "Null response",
                "application/json" -> mediaType(Fixed.reference("#/components/schemas/Owners")))
            ).withParameter(path("id", Fixed.string()))
          )
        )
      ) must ===("""|import models._
                    |import shapeless.{:+:, CNil}
                    |trait PetsClient[F[_]] {
                    |  import PetsClient._
                    |  def createPets(newPet: NewPet): F[Unit]
                    |  def updatePets(id: String, updatePet: UpdatePet): F[Unit]
                    |  def getOwnersPets(id: String): F[Owners]
                    |}
                    |object PetsClient {
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
    import client.http4s.print.impl
    import client.http4s.print.Http4sSpecifics
    implicit val none: Http4sSpecifics = new Http4sSpecifics {
      def none[A]                                     = Printer.unit.contramap[A](_ => ())
      def applyMethod: Printer[(TraitName, ImplName)] = none
      def withBody: Printer[String]                   = none

    }
    val printer = impl
    "when a put and delete are provided" >> {
      printer.print(openApi("Petstore").withPath(mediaTypeReferencePutDelete)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |
           |    def deletePayload(id: String): F[Unit] = client.expect[Unit](Request[F](method = Method.DELETE, uri = baseUrl / "payloads" / id.show))
           |    def updatePayload(id: String, updatePayload: UpdatePayload): F[Unit] = client.expect[Unit](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show))
           |  }
           |
           |}""".stripMargin)
    }

    "when get endpoints are provided" >> {
      printer.print(openApi("Petstore").withPath(mediaTypeReferenceGet).withPath(mediaTypeReferenceGetId)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |
           |    def getPayload(limit: Option[Int], name: Option[String]): F[Payloads] = client.expect[Payloads](Request[F](method = Method.GET, uri = baseUrl / "payloads" +?? ("limit", limit) +?? ("name", name)))
           |    def getPayload(id: String): F[Payload] = client.expect[Payload](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when optional body and not optional query parameters is provided" >> {
      printer.print(openApi("Petstore").withPath(mediaTypeOptionBody).withSchema("UpdatePayload" -> Fixed.string())) must ===(
        """|object PetstoreHttpClient {
           |  implicit val UpdatePayloadEncoder: Encoder[UpdatePayload] = deriveEncoder[UpdatePayload]
           |  implicit val OptionUpdatePayloadEncoder: Encoder[Option[UpdatePayload]] = deriveEncoder[Option[UpdatePayload]]
           |  implicit val UpdatePayloadDecoder: Decoder[UpdatePayload] = deriveDecoder[UpdatePayload]
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    implicit val UpdatePayloadEntityEncoder: EntityEncoder[F, UpdatePayload] = jsonEncoderOf[F, UpdatePayload]
           |    implicit val OptionUpdatePayloadEntityEncoder: EntityEncoder[F, Option[UpdatePayload]] = jsonEncoderOf[F, Option[UpdatePayload]]
           |    implicit val UpdatePayloadEntityDecoder: EntityDecoder[F, UpdatePayload] = jsonOf[F, UpdatePayload]
           |    def deletePayload(id: String, size: Long, updatePayload: Option[UpdatePayload]): F[Unit] = client.expect[Unit](Request[F](method = Method.DELETE, uri = baseUrl / "payloads" / id.show +? ("size", size)))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when references in the request and the responses" >> {
      printer.print(openApi("Petstore").withPath(references)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |
           |    def updatePayload(updatePayload: UpdatePayload): F[UpdatedPayload] = client.expect[UpdatedPayload](Request[F](method = Method.PUT, uri = baseUrl / "payloads"))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with a default one" >> {
      printer.print(openApi("Petstore").withPath(multipleResponsesWithDefaultOne)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |
           |    def getPayload(id: String): F[Either[GetPayloadError, Payload]] = client.fetch[Either[GetPayloadError, Payload]](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[Payload].map(x => Coproduct[Payload](x))
           |      case default => default.as[Error].map(x => Coproduct[UnexpectedErrorResponse](UnexpectedErrorResponse(default.status.code, x)))
           |    }
           |  }
           |
           |}""".stripMargin)
    }

    "when there are multiple responses with not found response" >> {
      printer.print(openApi("Petstore").withPath(notFoundResponse)) must ===(
        """|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |
           |    def getPayload(id: String): F[Either[GetPayloadError, Payload]] = client.fetch[Either[GetPayloadError, Payload]](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[Payload].map(x => Coproduct[Payload](x))
           |      case response if response.status.code == 404 => response.as[String].map(x => Coproduct[NotFoundError](NotFoundError(x)))
           |    }
           |  }
           |
           |}""".stripMargin)
    }

    "when there are simple response and response with anonymous objects" >> {
      printer.print(openApi("Petstore").withPath(simpleResponseResponseAnonymousObjects)) must ===(
        """|object PetstoreHttpClient {
          |
          |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
          |    import PetstoreClient._
          |
          |    def updateAnotherPayload(id: String, updateAnotherPayloadRequest: UpdateAnotherPayloadRequest): F[UpdatedPayload] = client.expect[UpdatedPayload](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show))
          |  }
          |
          |}""".stripMargin)
    }

    "when  multiple responses with anonymous objects with default response" >> {
      printer.print(openApi("Payload").withPath(multipleResponsesWithAnonymousObjectAndDefaultOne)) must ===(
        """|object PayloadHttpClient {
          |
          |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PayloadClient[F] = new PayloadClient[F] {
          |    import PayloadClient._
          |
          |    def updatePayload(id: String): F[Either[UpdatePayloadError, UpdatedPayload]] = client.fetch[Either[UpdatePayloadError, UpdatedPayload]](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show)) {
          |      case Successful(response) => response.as[UpdatedPayload].map(x => Coproduct[UpdatedPayload](x))
          |      case default => default.as[UnexpectedError].map(x => Coproduct[UnexpectedErrorResponse](UnexpectedErrorResponse(default.status.code, x)))
          |    }
          |  }
          |
          |}""".stripMargin)
    }
  }

  "http4s 0.20.x should able to print" >> {
    import client.print._
    import client.http4s.print.{openApi => printer}
    import client.http4s.print.v20._

    "when a post operation is provided" >> {
      printer.print(
        PackageName("petstore") -> openApi("Petstore")
          .withPath(mediaTypeReferencePost)
          .withSchema("NewPayload" -> Fixed.string())) must ===(
        """|import cats.effect._
           |import cats.syntax.functor._
           |import cats.syntax.either._
           |import cats.syntax.show._
           |import cats.implicits.catsStdShowForLong
           |import org.http4s._
           |import org.http4s.client.Client
           |import org.http4s.client.blaze._
           |import org.http4s.circe._
           |import org.http4s.Status.Successful
           |import io.circe._
           |import io.circe.generic.semiauto._
           |import shapeless.Coproduct
           |import scala.concurrent.ExecutionContext
           |import petstore.PetstoreClient
           |import petstore.models._
           |object PetstoreHttpClient {
           |  implicit val NewPayloadEncoder: Encoder[NewPayload] = deriveEncoder[NewPayload]
           |  implicit val OptionNewPayloadEncoder: Encoder[Option[NewPayload]] = deriveEncoder[Option[NewPayload]]
           |  implicit val NewPayloadDecoder: Decoder[NewPayload] = deriveDecoder[NewPayload]
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    implicit val NewPayloadEntityEncoder: EntityEncoder[F, NewPayload] = jsonEncoderOf[F, NewPayload]
           |    implicit val OptionNewPayloadEntityEncoder: EntityEncoder[F, Option[NewPayload]] = jsonEncoderOf[F, Option[NewPayload]]
           |    implicit val NewPayloadEntityDecoder: EntityDecoder[F, NewPayload] = jsonOf[F, NewPayload]
           |    def createPayload(newPayload: NewPayload): F[Unit] = client.expect[Unit](Request[F](method = Method.POST, uri = baseUrl / "payloads")).withBody(newPayload)
           |  }
           |  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext): Resource[F, PetstoreClient[F]] = BlazeClientBuilder(executionContext).resource.map(PetstoreHttpClient.build(_, baseUrl))
           |}""".stripMargin
      )
    }
  }

  "http4s 0.18.x should able to print" >> {
    import client.print._
    import client.http4s.print.{openApi => printer}
    import client.http4s.print.v18._

    "when a post operation is provided" >> {

      printer.print(
        PackageName("petstore") -> openApi("Petstore")
          .withPath(mediaTypeReferencePost)
          .withSchema("NewPayload" -> Fixed.string())) must ===(
        """|import cats.effect._
           |import cats.syntax.functor._
           |import cats.syntax.either._
           |import cats.syntax.show._
           |import cats.implicits.catsStdShowForLong
           |import org.http4s._
           |import org.http4s.client.Client
           |import org.http4s.client.blaze._
           |import org.http4s.circe._
           |import org.http4s.Status.Successful
           |import io.circe._
           |import io.circe.generic.semiauto._
           |import shapeless.Coproduct
           |import scala.concurrent.ExecutionContext
           |import petstore.PetstoreClient
           |import petstore.models._
           |object PetstoreHttpClient {
           |  implicit val NewPayloadEncoder: Encoder[NewPayload] = deriveEncoder[NewPayload]
           |  implicit val OptionNewPayloadEncoder: Encoder[Option[NewPayload]] = deriveEncoder[Option[NewPayload]]
           |  implicit val NewPayloadDecoder: Decoder[NewPayload] = deriveDecoder[NewPayload]
           |  def build[F[_]: Effect](client: Client[F], baseUrl: Uri): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |    implicit val NewPayloadEntityEncoder: EntityEncoder[F, NewPayload] = jsonEncoderOf[F, NewPayload]
           |    implicit val OptionNewPayloadEntityEncoder: EntityEncoder[F, Option[NewPayload]] = jsonEncoderOf[F, Option[NewPayload]]
           |    implicit val NewPayloadEntityDecoder: EntityDecoder[F, NewPayload] = jsonOf[F, NewPayload]
           |    def createPayload(newPayload: NewPayload): F[Unit] = client.expect[Unit](Request[F](method = Method.POST, uri = baseUrl / "payloads")).withEntity(newPayload)
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
  import client.print.TraitName
  import schema.Path.ItemObject

  private val pathId        = path("id", Fixed.string())
  private val payloadPath   = "/payloads"
  private val payloadPathId = s"$payloadPath/{id}"

  def paths[T](traitName: String = "PayloadClient")(
      xs: (String, ItemObject[T])*): (TraitName, Map[String, ItemObject[T]]) = TraitName(traitName) -> xs.toMap

  private val successPayloadResponse = "200" -> response(
    "Null response",
    "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payload"))
  )

  private val successNullResponse = "200" -> response[JsonSchemaF.Fixed]("Null response")

  val mediaTypeReferencePost = "/payloads" -> emptyItemObject.withPost(
    operation[JsonSchemaF.Fixed](
      request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPayload"))),
      responses = "201"          -> response("Null response")
    ).withOperationId("createPayload")
  )

  val mediaTypeReferencePutDelete = payloadPathId -> emptyItemObject
    .withPut(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePayload"))),
        successNullResponse
      ).withOperationId("updatePayload").withParameter(pathId))
    .withDelete(
      operation[JsonSchemaF.Fixed](
        request(),
        successNullResponse
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
      operationWithResponses[JsonSchemaF.Fixed](successPayloadResponse)
        .withOperationId("getPayload")
        .withParameter(path("id", Fixed.string())))

  val mediaTypeOptionBody = payloadPathId -> emptyItemObject
    .withDelete(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePayload"))).optional,
        successNullResponse
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
        responses = successPayloadResponse,
        "default" -> response(
          "Unexpected error",
          "application/json" -> mediaType(Fixed.reference("#/components/schemas/Error"))
        )
      ).withOperationId("getPayload").withParameter(path("id", Fixed.string()))
    )

  val notFoundResponse = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successPayloadResponse,
        "404" -> response(
          "Not found",
          "application/json" -> mediaType(Fixed.string())
        )
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

}
