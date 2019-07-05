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
import cats.implicits._

class JsonSchemaPrintSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  import print._

  "basic types should able to print" >> {
    "when integer is provided" >> {
      schema().print(Fixed.integer()) must ===("Int")
    }
    "when long is provided" >> {
      schema().print(Fixed.long()) must ===("Long")
    }
    "when float is provided" >> {
      schema().print(Fixed.float()) must ===("Float")
    }
    "when double is provided" >> {
      schema().print(Fixed.double()) must ===("Double")
    }
    "when string is provided" >> {
      schema().print(Fixed.string()) must ===("String")
    }
    "when byte is provided" >> {
      schema().print(Fixed.byte()) must ===("Array[Byte]")
    }
    "when binary is provided" >> {
      schema().print(Fixed.binary()) must ===("List[Boolean]")
    }
    "when boolean is provided" >> {
      schema().print(Fixed.boolean()) must ===("Boolean")
    }
    "when date is provided" >> {
      schema().print(Fixed.date()) must ===("java.time.LocalDate")
    }
    "when datetime is provided" >> {
      schema().print(Fixed.dateTime()) must ===("java.time.ZonedDateTime")
    }
    "when password is provided" >> {
      schema().print(Fixed.password()) must ===("String")
    }
  }

  "complex types should able to print" >> {
    "when an empty object is provided" >> {
      schema("Person".some)
        .print(Fixed.`object`(List.empty, List.empty)) must ===("type Person = io.circe.Json")
    }

    "when object is provided" >> {
      schema("Person".some)
        .print(
          Fixed.`object`(
            List(
              "name"    -> Fixed.string(),
              "surname" -> Fixed.string(),
              "age"     -> Fixed.integer(),
              "email"   -> Fixed.string()),
            List("name", "surname"))) must ===(
        "final case class Person(name: String, surname: String, age: Option[Int], email: Option[String])")
    }

    "when object is provided without required fields" >> {
      schema("Person".some)
        .print(Fixed.`object`(List("age" -> Fixed.integer(), "email" -> Fixed.string()), List("name", "surname"))) must ===(
        "final case class Person(age: Option[Int], email: Option[String])")
    }

    "when object is provided without optional fields" >> {
      schema("Person".some)
        .print(
          Fixed.`object`(
            List(
              "name"    -> Fixed.string(),
              "surname" -> Fixed.string()
            ),
            List("name", "surname"))) must ===("final case class Person(name: String, surname: String)")
    }

    "when enum is provided" >> {
      schema("Color".some)
        .print(Fixed.enum(List("blue", "red", "yellow"))) must ===("""
          |sealed trait Color
          |object Color {
          |  final case object Blue extends Color
          |  final case object Red extends Color
          |  final case object Yellow extends Color
          |}""".stripMargin)
    }
  }

}
