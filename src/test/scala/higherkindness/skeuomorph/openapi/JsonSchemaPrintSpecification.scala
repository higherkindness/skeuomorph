/*
 * Copyright 2018-2021 47 Degrees Open Source <https://www.47deg.com>
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
import higherkindness.skeuomorph.Printer.avoid._

class JsonSchemaPrintSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  import print.schemaWithName

  "basic types should able to print" >> {
    "when integer is provided" >> {
      print.schema().print(Fixed.integer()) must ===("Int")
    }
    "when long is provided" >> {
      print.schema().print(Fixed.long()) must ===("Long")
    }
    "when float is provided" >> {
      print.schema().print(Fixed.float()) must ===("Float")
    }
    "when double is provided" >> {
      print.schema().print(Fixed.double()) must ===("Double")
    }
    "when string is provided" >> {
      print.schema().print(Fixed.string()) must ===("String")
    }
    "when byte is provided" >> {
      print.schema().print(Fixed.byte()) must ===("Array[Byte]")
    }
    "when binary is provided" >> {
      print.schema().print(Fixed.binary()) must ===("List[Boolean]")
    }
    "when boolean is provided" >> {
      print.schema().print(Fixed.boolean()) must ===("Boolean")
    }
    "when date is provided" >> {
      print.schema().print(Fixed.date()) must ===("java.time.LocalDate")
    }
    "when datetime is provided" >> {
      print.schema().print(Fixed.dateTime()) must ===("java.time.ZonedDateTime")
    }
    "when password is provided" >> {
      print.schema().print(Fixed.password()) must ===("String")
    }
  }

  "complex types should able to print" >> {
    "when an empty object is provided" >> {
      schemaWithName
        .print("Person" -> Fixed.`object`(List.empty, List.empty)) must ===("type Person = io.circe.Json")
    }

    "when object is provided" >> {
      schemaWithName
        .print(
          "Person" ->
            Fixed.`object`(
              List(
                "name"    -> Fixed.string(),
                "surname" -> Fixed.string(),
                "age"     -> Fixed.integer(),
                "email"   -> Fixed.string()
              ),
              List("name", "surname")
            )
        ) must ===(s"""|final case class Person(name: String, surname: String, age: Option[Int], email: Option[String])
            |object Person {
            |
            |
            |}""".stripMargin)
    }

    "when object is provided whose name is a Scala reserved word" >> {
      schemaWithName
        .print(
          "=>" ->
            Fixed.`object`(List("name" -> Fixed.string()), List("name"))
        ) must ===(s"""|final case class `=>`(name: String)
            |object `=>` {
            |
            |
            |}""".stripMargin)
    }

    "when object is provided without required fields" >> {
      schemaWithName
        .print(
          "Person" -> Fixed
            .`object`(List("age" -> Fixed.integer(), "email" -> Fixed.string()), List("name", "surname"))
        ) must ===(s"""|final case class Person(age: Option[Int], email: Option[String])
            |object Person {
            |
            |
            |}""".stripMargin)
    }

    "when object is provided without optional fields" >> {
      schemaWithName
        .print(
          "Person" ->
            Fixed.`object`(
              List(
                "name"    -> Fixed.string(),
                "surname" -> Fixed.string()
              ),
              List("name", "surname")
            )
        ) must ===(s"""|final case class Person(name: String, surname: String)
                                                   |object Person {
                                                   |
                                                   |
                                                   |}""".stripMargin)
    }

    "when object is provided with a field name which is a Scala reserved word" >> {
      schemaWithName
        .print(
          "Person" ->
            Fixed.`object`(
              List(
                "name" -> Fixed.string(),
                "type" -> Fixed.string()
              ),
              List("name", "type")
            )
        ) must ===(s"""|final case class Person(name: String, `type`: String)
                                                   |object Person {
                                                   |
                                                   |
                                                   |}""".stripMargin)
    }

    "when enum is provided" >> {
      schemaWithName
        .print("Color" -> Fixed.enum(List("Blue", "Red", "Yellow"))) must
        ===("""|sealed trait Color
               |object Color {
               |
               |  final case object Blue extends Color
               |  final case object Red extends Color
               |  final case object Yellow extends Color
               |
               |}""".stripMargin)
    }

    "when sum is provided" >> {
      schemaWithName
        .print(
          "Pet" -> Fixed
            .sum(List(Fixed.reference("#/components/schemas/Dog"), Fixed.reference("#/components/schemas/Cat")))
        ) must
        ===("""|type Pet = Dog :+: Cat :+: CNil
               |object Pet {
               |
               |
               |}""".stripMargin)

    }
  }
}
