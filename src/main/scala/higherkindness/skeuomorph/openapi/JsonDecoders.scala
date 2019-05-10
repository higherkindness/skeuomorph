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

import schema._
import io.circe.Decoder
import cats.implicits._

object JsonDecoders {

  implicit val referenceDecoder: Decoder[Reference] = Decoder.forProduct1(s"$$ref")(Reference.apply)

  implicit def orReferenceDecoder[A: Decoder]: Decoder[Either[A, Reference]] =
    Decoder[Reference].map(_.asRight[A]) orElse Decoder[A].map(_.asLeft[Reference])

  private def basicJsonSchemaDecoder: Decoder[JsonSchemaF.Fixed] = {
    import JsonSchemaF.Fixed._
    Decoder.forProduct1[String, String]("type") { identity }.emap {
      case "integer"  => integer().asRight
      case "long"     => long().asRight
      case "float"    => float().asRight
      case "double"   => double().asRight
      case "string"   => string().asRight
      case "byte"     => byte().asRight
      case "binary"   => binary().asRight
      case "boolean"  => boolean().asRight
      case "date"     => date().asRight
      case "datetime" => dateTime().asRight
      case "password" => password().asRight
      case x          => s"$x is not well formed type".asLeft
    }
  }

  private def enumJsonSchemaDecoder: Decoder[JsonSchemaF.Fixed] =
    Decoder
      .forProduct2[(String, List[String]), String, List[String]]("type", "enum")(Tuple2.apply)
      .emap {
        case ("string", values) => JsonSchemaF.Fixed.enum(values).asRight
        case x                  => s"$x is not valid enum".asLeft
      }

  private def objectJsonSchemaDecoder: Decoder[JsonSchemaF.Fixed] =
    Decoder
      .forProduct3[
        (String, Map[String, JsonSchemaF.Fixed], List[String]),
        String,
        Map[String, JsonSchemaF.Fixed],
        List[String]]("type", "properties", "required")(Tuple3.apply)
      .emap {
        case ("object", properties, required) =>
          JsonSchemaF.Fixed.`object`(properties.toList, required).asRight
        case x =>
          s"$x is not valid object".asLeft
      }

  private def arrayJsonSchemaDecoder: Decoder[JsonSchemaF.Fixed] =
    Decoder
      .forProduct2[(String, JsonSchemaF.Fixed), String, JsonSchemaF.Fixed]("type", "items")(Tuple2.apply)
      .emap {
        case ("array", x) =>
          JsonSchemaF.Fixed.array(x).asRight
        case x => s"$x is not an array".asLeft
      }
  private def referenceJsonSchemaDecoder: Decoder[JsonSchemaF.Fixed] =
    Decoder[Reference].map(x => JsonSchemaF.Fixed.reference(x.ref))

  implicit val jsonSchemaDecoder: Decoder[JsonSchemaF.Fixed] =
    referenceJsonSchemaDecoder orElse
      objectJsonSchemaDecoder orElse
      arrayJsonSchemaDecoder orElse
      enumJsonSchemaDecoder orElse
      basicJsonSchemaDecoder

  implicit val infoDecoder: Decoder[Info] =
    Decoder.forProduct3(
      "title",
      "description",
      "version"
    )(Info.apply)

  implicit val serverVariableDecoder: Decoder[Server.Variable] =
    Decoder.forProduct3(
      "enum",
      "default",
      "description"
    )((enum: Option[List[String]], default: String, description: Option[String]) =>
      Server.Variable(enum.getOrElse(List.empty), default, description))

  implicit val serverDecoder: Decoder[Server] =
    Decoder.forProduct3(
      "url",
      "description",
      "variables"
    ) { (url: String, description: Option[String], variables: Option[Map[String, Server.Variable]]) =>
      Server(url, description, variables.getOrElse(Map.empty))
    }

  implicit val externalDocsDecoder: Decoder[ExternalDocs] =
    Decoder.forProduct2(
      "url",
      "description"
    )(ExternalDocs.apply)

  implicit val tagDecoder: Decoder[Tag] =
    Decoder.forProduct3(
      "name",
      "description",
      "externalDocs"
    )(Tag.apply)

  implicit def headerDecoder[A: Decoder]: Decoder[Header[A]] =
    Decoder.forProduct2(
      "description",
      "schema"
    )(Header.apply)

  implicit def encodingDecoder[A: Decoder]: Decoder[Encoding[A]] =
    Decoder.forProduct5(
      "contentType",
      "headers",
      "style",
      "explode",
      "allowReserved"
    )(
      (
          contentType: Option[String],
          headers: Option[Map[String, Either[Header[A], Reference]]],
          style: Option[String],
          explode: Option[Boolean],
          allowReserved: Option[Boolean]) =>
        Encoding(contentType, headers.getOrElse(Map.empty), style, explode, allowReserved))

  implicit def mediaTypeDecoder[A: Decoder]: Decoder[MediaType[A]] =
    Decoder.forProduct2(
      "schema",
      "encoding"
    )((schema: Option[A], encoding: Option[Map[String, Encoding[A]]]) =>
      MediaType(schema, encoding.getOrElse(Map.empty)))

  implicit def requestDecoder[A: Decoder]: Decoder[Request[A]] =
    Decoder.forProduct3(
      "description",
      "content",
      "required"
    )((description: Option[String], content: Map[String, MediaType[A]], required: Option[Boolean]) =>
      Request(description, content, required.getOrElse(false)))

  implicit def responseDecoder[A: Decoder]: Decoder[Response[A]] =
    Decoder.forProduct3(
      "description",
      "headers",
      "content"
    )(
      (
          description: String,
          headers: Option[Map[String, Either[Header[A], Reference]]],
          content: Option[Map[String, MediaType[A]]]) =>
        Response(description, headers.getOrElse(Map.empty), content.getOrElse(Map.empty)))

  implicit val locationDecoder: Decoder[Location] = Decoder.decodeString.emap(Location.parse)

  implicit def parameterDecoder[A: Decoder]: Decoder[Parameter[A]] =
    Decoder.forProduct10(
      "name",
      "in",
      "description",
      "required",
      "deprecated",
      "style",
      "explode",
      "allowEmptyValue",
      "allowReserved",
      "schema"
    )(Parameter.apply)

  // Avoid forProductXX
  implicit def operationDecoder[A: Decoder]: Decoder[Path.Operation[A]] =
    Decoder.instance(
      c =>
        for {
          tags         <- c.downField("tags").as[Option[List[String]]]
          summary      <- c.downField("summary").as[Option[String]]
          description  <- c.downField("description").as[Option[String]]
          externalDocs <- c.downField("externalDocs").as[Option[ExternalDocs]]
          operationId  <- c.downField("operationId").as[Option[String]]
          parameters   <- c.downField("parameters").as[Option[List[Either[Parameter[A], Reference]]]]
          requestBody  <- c.downField("requestBody").as[Either[Request[A], Reference]]
          responses    <- c.downField("responses").as[Map[String, Either[Response[A], Reference]]]
          callbacks    <- c.downField("callbacks").as[Option[Map[String, Either[Callback[A], Reference]]]]
          deprecated   <- c.downField("deprecated").as[Option[Boolean]]
          servers      <- c.downField("servers").as[Option[List[Server]]]
        } yield
          Path.Operation.apply(
            tags.getOrElse(List.empty),
            summary,
            description,
            externalDocs,
            operationId,
            parameters.getOrElse(List.empty),
            requestBody,
            responses,
            callbacks.getOrElse(Map.empty),
            deprecated.getOrElse(false),
            servers.getOrElse(List.empty)
        ))

  implicit def itemObjectDecoder[A: Decoder]: Decoder[Path.ItemObject[A]] =
    Decoder.forProduct12(
      "ref",
      "summary",
      "description",
      "get",
      "put",
      "post",
      "delete",
      "options",
      "head",
      "patch",
      "trace",
      "servers")(
      (
          (
              ref: Option[String], // $ref
              summary: Option[String],
              description: Option[String],
              get: Option[Path.Operation[A]],
              put: Option[Path.Operation[A]],
              post: Option[Path.Operation[A]],
              delete: Option[Path.Operation[A]],
              options: Option[Path.Operation[A]],
              head: Option[Path.Operation[A]],
              patch: Option[Path.Operation[A]],
              trace: Option[Path.Operation[A]],
              servers: Option[List[Server]]) =>
            Path.ItemObject(
              ref,
              summary,
              description,
              get,
              put,
              post,
              delete,
              options,
              head,
              patch,
              trace,
              servers.getOrElse(List.empty))))

  implicit def componentsDecoder[A: Decoder]: Decoder[Components[A]] =
    Decoder.forProduct2(
      "responses",
      "requestBodies"
    )(
      (
          responses: Option[Map[String, Either[Response[A], Reference]]],
          requestBodies: Option[Map[String, Either[Request[A], Reference]]]) =>
        Components(responses.getOrElse(Map.empty), requestBodies.getOrElse(Map.empty)))

  implicit def openApiDecoder[A: Decoder]: Decoder[OpenApi[A]] =
    Decoder.forProduct7(
      "openapi",
      "info",
      "servers",
      "paths",
      "components",
      "tags",
      "externalDocs"
    )(
      (
          openapi: String,
          info: Info,
          servers: Option[List[Server]],
          paths: Map[String, Path.ItemObject[A]],
          components: Option[Components[A]],
          tags: Option[List[Tag]],
          externalDocs: Option[ExternalDocs]) =>
        OpenApi.apply(
          openapi,
          info,
          servers.getOrElse(List.empty),
          paths,
          components,
          tags.getOrElse(List.empty),
          externalDocs))

}
