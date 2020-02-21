/*
 * Copyright 2018-2020 47 Degrees, LLC. <http://www.47deg.com>
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

trait OperationHelpers {
  import cats.implicits._
  import schema._

  def operationWithReferences[A](request: Reference, responses: (String, Reference)*): Path.Operation[A] =
    operationFrom(request.asRight.some, responses.toMap.view.mapValues(_.asRight).toMap)

  def operationWithResponses[A](responses: (String, Response[A])*): Path.Operation[A] =
    operationFrom(none, responses.view.toMap.mapValues(_.asLeft).toMap)

  def operation[A](request: Request[A], responses: (String, Response[A])*): Path.Operation[A] =
    operationFrom(request.asLeft.some, responses.toMap.view.mapValues(_.asLeft).toMap)

  private def operationFrom[A](
      requestBody: Option[Either[Request[A], Reference]],
      responses: Map[String, Either[Response[A], Reference]]
  ): Path.Operation[A] =
    Path.Operation[A](
      tags = List.empty,
      summary = None,
      description = None,
      externalDocs = None,
      operationId = None,
      parameters = List.empty,
      requestBody = requestBody,
      responses = responses,
      callbacks = Map.empty,
      deprecated = false,
      servers = List.empty
    )

}
