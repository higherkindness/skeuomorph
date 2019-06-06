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

class OpenApiPrintSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  import print._
  import helpers._
  import cats.implicits._

  "models should able to print" >> {
    "when a basic type is provided" >> {
      model.print(components("Foo" -> Fixed.string())) must ===("""|object models {
            |  type Foo = String
            |}""".stripMargin)
    }

    "when a object type is provided" >> {
      model.print(components("Foo" -> Fixed.`object`(List("bar" -> Fixed.string()), List.empty))) must ===(
        """|object models {
              |  final case class Foo (bar: Option[String])
              |}""".stripMargin)
    }

    "when multiple types are provided" >> {
      model.print(
        components(
          "Bar"  -> Fixed.`object`(List("foo" -> Fixed.string()), List("foo")),
          "Bars" -> Fixed.array(Fixed.reference("#/components/schemas/Bar")))) must ===(
        """|object models {
                |  final case class Bar (foo: String)
                |  type Bars = List[Bar]
                |}""".stripMargin)
    }
  }

  "Client trait should able to print" >> {
    import client.print._
    "when a post operation is provided" >> {
      operations.print(
        "Payload" -> Map(
          "/payloads" -> post(
            operation[JsonSchemaF.Fixed](
              request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPayload"))),
              responses = "201"          -> response("Null response").asLeft
            ).withOperationId("createPayload")))) must ===("""|trait PayloadClient[F[_]] {
              |  import PayloadClient._
              |  def createPayload(newPayload: NewPayload): F[Unit]
              |}
              |object PayloadClient {
              |
              |}""".stripMargin)
    }

    import client.print._
    "when a put and delete are provided" >> {
      operations.print(
        "Payload" -> Map(
          "/payloads" -> emptyItemObject
            .withPut(
              operation[JsonSchemaF.Fixed](
                request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePayload"))),
                responses = "200"          -> response("Null response").asLeft
              ).withOperationId("updatePayload"))
            .withDelete(operation[JsonSchemaF.Fixed](
              request(),
              responses = "200" -> response("Null response").asLeft
            ).withOperationId("deletePayload"))
        )) must ===("""|trait PayloadClient[F[_]] {
              |  import PayloadClient._
              |  def deletePayload(): F[Unit]
              |  def updatePayload(updatePayload: UpdatePayload): F[Unit]
              |}
              |object PayloadClient {
                |
                |}""".stripMargin)
    }
    // |  def deletePayload(): F[Unit]
  }

}
