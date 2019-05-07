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
import io.circe.{Encoder, Json}
import qq.droste._
import qq.droste.syntax.all._

object JsonEncoders {

  implicit def referenceEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Reference] =
    jsonSchemaEncoder.contramap[Reference](x => JsonSchemaF.reference(x.ref))

  implicit def orReferenceEncoder[A: Encoder]: Encoder[Either[A, Reference]] =
    Encoder.instance[Either[A, Reference]](_.fold(Encoder[A].apply, Encoder[Reference].apply))
  implicit val infoEncoder: Encoder[Info] =
    Encoder.forProduct3(
      "title",
      "description",
      "version"
    )(i => (i.title, i.description, i.version))

  implicit val serverVariableEncoder: Encoder[Server.Variable] =
    Encoder.forProduct3(
      "enum",
      "default",
      "description"
    )(v => (v.enum, v.default, v.description))

  implicit val serverEncoder: Encoder[Server] =
    Encoder.forProduct3(
      "url",
      "description",
      "variables"
    )(s => (s.url, s.description, s.variables))

  implicit val externalDocsEncoder: Encoder[ExternalDocs] =
    Encoder.forProduct2(
      "url",
      "description"
    )(d => (d.url, d.description))

  implicit val tagEncoder: Encoder[Tag] =
    Encoder.forProduct3(
      "name",
      "description",
      "externalDocs"
    )(t => (t.name, t.description, t.externalDocs))

  implicit def jsonSchemaEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[JsonSchemaF[A]] =
    Encoder.instance(sch => scheme.cata[JsonSchemaF, A, Json](JsonSchemaF.render).apply(sch.embed))

  implicit def headerEncoder[A: Encoder]: Encoder[Header[A]] =
    Encoder.forProduct2(
      "description",
      "schema"
    ) { h =>
      (h.description, h.schema)
    }

  implicit def encodingEncoder[A: Encoder]: Encoder[Encoding[A]] =
    Encoder.forProduct5(
      "contentType",
      "headers",
      "style",
      "explode",
      "allowReserved"
    ) { e =>
      (e.contentType, e.headers, e.style, e.explode, e.allowReserved)
    }

  implicit def mediaTypeEncoder[A: Encoder]: Encoder[MediaType[A]] =
    Encoder.forProduct2(
      "schema",
      "encoding"
    ) { m =>
      (m.schema, m.encoding)
    }

  implicit def requestEncoder[A: Encoder]: Encoder[Request[A]] =
    Encoder.forProduct3(
      "description",
      "content",
      "required"
    )(r => (r.description, r.content, r.required))

  implicit def responseEncoder[A: Encoder]: Encoder[Response[A]] =
    Encoder.forProduct3(
      "description",
      "headers",
      "content"
    ) { r =>
      (r.description, r.headers, r.content)
    }

  implicit val locationEncoder: Encoder[Location] = Encoder.encodeString.contramap(_.value)

  implicit def parameterEncoder[A: Encoder]: Encoder[Parameter[A]] =
    Encoder.forProduct10(
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
    ) { p =>
      (
        p.name,
        p.in,
        p.description,
        p.required,
        p.deprecated,
        p.style,
        p.explode,
        p.allowEmptyValue,
        p.allowReserved,
        p.schema)
    }

  // TODO Review: Using forProduct10 produce recursive call
  implicit def operationEncoder[A: Encoder]: Encoder[Path.Operation[A]] =
    Encoder.instance { op =>
      import io.circe.syntax._
      Json.obj(
        "tags"         -> op.tags.asJson,
        "summary"      -> op.summary.asJson,
        "description"  -> op.description.asJson,
        "externalDocs" -> op.externalDocs.asJson,
        "operationId"  -> op.operationId.asJson,
        "parameters"   -> op.parameters.asJson,
        "responses"    -> op.responses.asJson,
        // "callbacks"    -> op.callbacks.asJson,
        "deprecated" -> op.deprecated.asJson,
        "servers"    -> op.servers.asJson
      )
    }

  implicit def itemObjectEncoder[A: Encoder]: Encoder[Path.ItemObject[A]] =
    Encoder.forProduct12(
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
    ) { i =>
      (i.ref, i.summary, i.description, i.get, i.put, i.post, i.delete, i.options, i.head, i.patch, i.trace, i.servers)
    }

  // (implicit A: Basis[JsonSchemaF, A])
  implicit def componentsEncoder[A: Encoder]: Encoder[Components[A]] =
    Encoder.forProduct2(
      "responses",
      "requestBodies"
    )(c => (c.responses, c.requestBodies))

  implicit def openApiEncoder[A: Encoder]: Encoder[OpenApi[A]] =
    Encoder.forProduct7(
      "openapi",
      "info",
      "servers",
      "paths",
      "components",
      "tags",
      "externalDocs"
    ) { o =>
      (o.openapi, o.info, o.servers, o.paths, o.components, o.tags, o.externalDocs)
    }
}
