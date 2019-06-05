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
  import higherkindness.skeuomorph.openapi.schema._

  "components should able to print" >> {
    "when a basic type is provided" >> {
      model.print(modelFrom("Foo" -> Fixed.string())) must ===("""|object models {
            |  type Foo = String
            |}""".stripMargin)
    }

    "when a object type is provided" >> {
      model.print(modelFrom("Foo" -> Fixed.`object`(List("bar" -> Fixed.string()), List.empty))) must ===(
        """|object models {
              |  final case class Foo (bar: Option[String])
              |}""".stripMargin)
    }

    "when multiple types are provided" >> {
      model.print(
        modelFrom(
          "Bar"  -> Fixed.`object`(List("foo" -> Fixed.string()), List("foo")),
          "Bars" -> Fixed.array(Fixed.reference("#/components/schemas/Bar")))) must ===(
        """|object models {
                |  final case class Bar (foo: String)
                |  type Bars = List[Bar]
                |}""".stripMargin)
    }
  }

  def modelFrom[T](models: (String, T)*): Components[T] = Components(models.toMap, Map.empty, Map.empty)
}
