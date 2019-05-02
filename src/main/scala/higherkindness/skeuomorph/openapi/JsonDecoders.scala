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

  implicit val referenceDecoder: Decoder[Reference] = Decoder.forProduct1("$ref")(Reference.apply)

  implicit def orReferenceDecoder[A: Decoder]: Decoder[Either[A, Reference]] =
    Decoder[Reference].map(_.asRight[A]).handleErrorWith(_ => Decoder[A].map(_.asLeft[Reference]))

  implicit def jsonSchemaDecoder[A]: Decoder[JsonSchemaF[A]] =
    // import qq.droste.data.Fix
    // Decoder[Json].map(scheme.ana(JsonSchemaF.fromJson)).map[JsonSchemaF[A]](x => Fix.un[JsonSchemaF](x))
    ???

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
    )(Server.Variable.apply)

  implicit val serverDecoder: Decoder[Server] =
    Decoder.forProduct3(
      "url",
      "description",
      "variables"
    )(Server.apply)

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

  implicit def headerDecoder[A]: Decoder[Header[A]] =
    Decoder.forProduct2(
      "description",
      "schema"
    )(Header.apply)

  implicit def encodingDecoder[A]: Decoder[Encoding[A]] =
    Decoder.forProduct5(
      "contentType",
      "headers",
      "style",
      "explode",
      "allowReserved"
    )(Encoding.apply)

  implicit def mediaTypeDecoder[A]: Decoder[MediaType[A]] =
    Decoder.forProduct2(
      "schema",
      "encoding"
    )(MediaType.apply)

  implicit def requestDecoder[A]: Decoder[Request[A]] =
    Decoder.forProduct3(
      "description",
      "content",
      "required"
    )(Request.apply)

  implicit def responseDecoder[A]: Decoder[Response[A]] =
    Decoder.forProduct3(
      "description",
      "headers",
      "content"
    )(Response.apply)

  implicit val locationDecoder: Decoder[Location] = Decoder.decodeString.emap(Location.parse)

  implicit def parameterDecoder[A]: Decoder[Parameter[A]] =
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

  implicit def operationDecoder[A]: Decoder[Path.Operation[A]] = {
    Decoder.forProduct10(
      "tags",
      "summary",
      "description",
      "externalDocs",
      "operationId",
      "parameters",
      "responses",
      "callbacks",
      "deprecated",
      "servers"
    )(Path.Operation.apply[A])
  }

  implicit def itemObjectDecoder[A]: Decoder[Path.ItemObject[A]] =
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
      "servers"
    )(Path.ItemObject.apply)

  implicit def componentsDecoder[A]: Decoder[Components[A]] =
    Decoder.forProduct2(
      "responses",
      "requestBodies"
    )(Components.apply)

  implicit def openApiDecoder[A]: Decoder[OpenApi[A]] =
    Decoder.forProduct7(
      "openapi",
      "info",
      "servers",
      "paths",
      "components",
      "tags",
      "externalDocs"
    )(OpenApi.apply)

}
