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

object helpers {
  import io.circe.parser._
  import io.circe.Json
  import cats.implicits._
  import schema._

  def unsafeParse: String => Json = parse(_).valueOr(x => sys.error(x.message))

  def response[A](description: String, content: (String, MediaType[A])*): Response[A] =
    Response[A](description, Map.empty, content.toMap)

  def operationWithReferences[A](request: Reference, responses: (String, Reference)*): Path.Operation[A] =
    operationFrom(request.asRight.some, responses.toMap.mapValues(_.asRight))

  def operationWithResponses[A](responses: (String, Response[A])*): Path.Operation[A] =
    operationFrom(none, responses.toMap.mapValues(_.asLeft))

  def operation[A](request: Request[A], responses: (String, Response[A])*): Path.Operation[A] =
    operationFrom(request.asLeft.some, responses.toMap.mapValues(_.asLeft))

  def reference(value: String): Reference = Reference(value)

  private def operationFrom[A](
      requestBody: Option[Either[Request[A], Reference]],
      responses: Map[String, Either[Response[A], Reference]]): Path.Operation[A] =
    Path.Operation[A](
      List.empty,
      None,
      None,
      None,
      None,
      List.empty,
      requestBody,
      responses,
      Map.empty,
      false,
      List.empty
    )
  implicit class OperationOps[A](operation: Path.Operation[A]) {
    def withParameter(parameter: Parameter[A]): Path.Operation[A] =
      operation.copy(parameters = operation.parameters :+ parameter.asLeft)
    def withParameter(reference: Reference): Path.Operation[A] =
      operation.copy(parameters = operation.parameters :+ reference.asRight)
    def withOperationId(operationId: String): Path.Operation[A] =
      operation.copy(operationId = operationId.some)
    def withTag(tag: String): Path.Operation[A] =
      operation.copy(tags = operation.tags :+ tag)
    def withSummary(summary: String): Path.Operation[A] =
      operation.copy(summary = summary.some)
  }

  def request[A](content: (String, MediaType[A])*): Request[A] =
    Request[A](
      None,
      content.toMap,
      true
    )

  implicit class RequestOps[A](request: Request[A]) {
    def withDescription(description: String): Request[A] = request.copy(description = description.some)
    def optional: Request[A]                             = request.copy(required = false)
  }

  def path[A](name: String, schema: A, description: Option[String] = None): Parameter[A] =
    Parameter.Path(name = name, description = description, schema = schema)
  def query[A](
      name: String,
      schema: A,
      allowEmptyValue: Boolean = false,
      required: Boolean = false,
      description: Option[String] = None): Parameter[A] =
    Parameter.Query(
      name = name,
      description = description,
      allowEmptyValue = allowEmptyValue,
      schema = schema,
      required = required)

  def noneMediaType[A] = MediaType[A](None, Map.empty)

  def mediaType[A](a: A) = MediaType[A](a.some, Map.empty)

  def emptyItemObject[A] = Path.ItemObject[A](
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    List.empty
  )

  def obj(properties: (String, JsonSchemaF.Fixed)*)(required: String*): JsonSchemaF.Fixed =
    JsonSchemaF.Fixed.`object`(properties.toList, required.toList)
  implicit class ItemObjectOps[A](item: Path.ItemObject[A]) {
    def withDelete(operation: Path.Operation[A]): Path.ItemObject[A] =
      item.copy(delete = operation.some)
    def withPut(operation: Path.Operation[A]): Path.ItemObject[A] =
      item.copy(put = operation.some)
    def withPost(operation: Path.Operation[A]): Path.ItemObject[A] =
      item.copy(post = operation.some)
    def withGet(operation: Path.Operation[A]): Path.ItemObject[A] =
      item.copy(get = operation.some)
  }

  def components[T](models: (String, T)*): Components[T] = Components(models.toMap, Map.empty, Map.empty)

  def openApi[A](name: String, version: String = "0.0.0"): OpenApi[A] = OpenApi(
    "",
    Info(name, None, version),
    List.empty,
    Map.empty,
    None,
    List.empty,
    None
  )

  implicit class OpenApiOps[A](openApi: OpenApi[A]) {
    def withPath(path: (String, Path.ItemObject[A])): OpenApi[A] =
      openApi.copy(paths = openApi.paths.+(path))
    def withSchema(model: (String, A)): OpenApi[A] =
      openApi.copy(
        components = openApi.components.fold(components(model))(x => x.copy(x.schemas + model)).some
      )
  }

}
