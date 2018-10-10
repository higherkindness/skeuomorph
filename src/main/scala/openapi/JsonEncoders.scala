/*
 * Copyright 2018 47 Degrees, LLC. <http://www.47deg.com>
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

package skeuomorph
package openapi

import schema._
import qq.droste._
import qq.droste.syntax.all._
import io.circe.{Encoder, Json}

object JsonEncoders {

  implicit val referenceEncoder: Encoder[Reference] =
    Encoder.instance(
      r =>
        Json.obj(
          "$$ref" -> Json.fromString(r.ref)
      ))

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

  implicit def headerEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Header[A]] =
    Encoder.forProduct2(
      "description",
      "schema"
    ) { h =>
      (h.description, h.schema)
    }

  implicit def encodingEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Encoding[A]] =
    Encoder.forProduct5(
      "contentType",
      "headers",
      "style",
      "explode",
      "allowReserved"
    ) { e =>
      (e.contentType, e.headers, e.style, e.explode, e.allowReserved)
    }

  implicit def mediatypeEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[MediaType[A]] =
    Encoder.forProduct2(
      "schema",
      "encoding"
    ) { m =>
      (m.schema, m.encoding)
    }

  implicit def requestEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Request[A]] =
    Encoder.forProduct3(
      "description",
      "content",
      "required"
    )(r => (r.description, r.content, r.required))

  implicit def responseEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Response[A]] =
    Encoder.forProduct3(
      "description",
      "headers",
      "content"
    ) { r =>
      (r.description, r.headers, r.content)
    }

  implicit def operationEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Path.Operation[A]] =
    Encoder.forProduct9(
      "tags",
      "summary",
      "description",
      "externalDocs",
      "operationId",
      "responses",
      "callbacks",
      "deprecated",
      "servers") { op =>
      (
        op.tags,
        op.summary,
        op.description,
        op.externalDocs,
        op.operationId,
        op.responses,
        op.callbacks,
        op.deprecated,
        op.servers)
    }

  implicit def itemObjectEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Path.ItemObject[A]] =
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

  implicit def componentsEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[Components[A]] =
    Encoder.forProduct2(
      "responses",
      "requestBodies"
    )(c => (c.responses, c.requestBodies))

  implicit def openApiEncoder[A](implicit A: Basis[JsonSchemaF, A]): Encoder[OpenApi[A]] =
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
