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

/**
 * @see https://swagger.io/specification/
 */
object schema {

  case class OpenApi[A](
      openapi: String,
      info: Info,
      servers: List[Server],
      paths: Map[String, Path.ItemObject[A]],
      components: Option[Components[A]],
      tags: Option[List[Tag]],
      externalDocs: Option[ExternalDocs])

  case class Info(title: String, description: Option[String], version: String)

  case class Server(url: String, description: String, variables: Map[String, Server.Variable])

  object Server {
    case class Variable(enum: List[String], default: String, description: Option[String])
  }

  object Path {
    case class ItemObject[A](
        ref: String, // $ref
        summary: String,
        description: String,
        get: Option[Operation[A]],
        put: Option[Operation[A]],
        post: Option[Operation[A]],
        delete: Option[Operation[A]],
        options: Option[Operation[A]],
        head: Option[Operation[A]],
        patch: Option[Operation[A]],
        trace: Option[Operation[A]],
        servers: List[Server])

    case class Operation[A](
        tags: List[String],
        summary: String,
        description: String,
        externalDocs: ExternalDocs,
        operationId: String,
        responses: Map[String, Either[Response[A], Reference]],
        callbacks: Map[String, Either[Callback[A], Reference]],
        deprecated: Boolean,
        servers: List[Server])
  }

  case class Components[A](
      responses: Map[String, Either[Response[A], Reference]],
      requestBodies: Map[String, Either[Request[A], Reference]]
  )

  case class Request[A](description: String, content: Map[String, MediaType[A]], required: Boolean)

  case class MediaType[A](schema: Either[JsonSchemaF[A], Reference], encoding: Map[String, Encoding[A]])

  case class Encoding[A](
      contentType: String,
      headers: Map[String, Either[Header[A], Reference]],
      style: String,
      explode: Boolean,
      allowReserved: Boolean)

  case class Response[A](
      description: String,
      headers: Map[String, Either[Header[A], Reference]],
      content: Map[String, MediaType[A]])

  case class Tag(name: String, description: Option[String], externalDocs: Option[ExternalDocs])

  case class ExternalDocs(url: String, description: Option[String])

  type Callback[A] = Map[String, Path.ItemObject[A]]

  case class Reference(ref: String) // $ref

  case class Header[A](description: String, schema: Either[JsonSchemaF[A], Reference])

}
