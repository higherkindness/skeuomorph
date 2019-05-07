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

import io.circe.Decoder
import JsonDecoders._
import io.circe.Json
import cats.implicits._

class JsonSchemaDecoderSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  val decoder = Decoder[JsonSchemaF.Fixed]

  "Decoder[JsonSchemaF.Fixed] should able to decode" >> {
    "when integer object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("integer"))) must beRight(Fixed.integer())
    }
    "when long object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("long"))) must beRight(Fixed.long())
    }
    "when float object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("float"))) must beRight(Fixed.float())
    }
    "when double object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("double"))) must beRight(Fixed.double())
    }
    "when string object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("string"))) must beRight(Fixed.string())
    }
    "when byte object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("byte"))) must beRight(Fixed.byte())
    }

    "when binary object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("binary"))) must beRight(Fixed.binary())
    }
    "when boolean object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("boolean"))) must beRight(Fixed.boolean())
    }
    "when date object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("date"))) must beRight(Fixed.date())
    }
    "when datetime object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("datetime"))) must beRight(Fixed.dateTime())
    }
    "when password object is provided" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("password"))) must beRight(Fixed.password())
    }

    "when reference object is provided" >> {
      decoder.decodeJson(Json.obj(s"$$ref" -> Json.fromString("/my/ref"))) must beRight(Fixed.reference("/my/ref"))
    }
    "when enum object is provided" >> {
      val enumType = Json.obj(
        "type" -> Json.fromString("string"),
        "enum" -> Json.arr(
          Json.fromString("green"),
          Json.fromString("blue")
        ))
      decoder.decodeJson(enumType) must beRight(Fixed.enum(List("green", "blue")))
    }
    "when array object is provided" >> {
      val arrayType = Json.obj(
        "type" -> Json.fromString("array"),
        "values" -> Json.obj(
          "type" -> Json.fromString("integer")
        )
      )
      decoder.decodeJson(arrayType) must beRight(Fixed.array(Fixed.integer()))
    }

    "when object object is provided" >> {
      val objectType = Json.obj(
        "type" -> Json.fromString("object"),
        "properties" -> Json.obj(
          "name" -> Json.obj("type" -> Json.fromString("string")),
          "age" -> Json.obj(
            "type"    -> Json.fromString("integer"),
            "format"  -> Json.fromString("int32"),
            "minimum" -> Json.fromInt(0)),
          "address" -> Json.obj(s"$$ref" -> Json.fromString("#/components/schemas/Address"))
        ),
        "required" -> Json.arr(Json.fromString("name"))
      )
      decoder.decodeJson(objectType) must beRight(
        Fixed.`object`(
          List(
            "name"    -> Fixed.string(),
            "age"     -> Fixed.integer(),
            "address" -> Fixed.reference("#/components/schemas/Address")),
          List("name")
        ))
    }
  }

  "Decoder[JsonSchemaF.Fixed] should not able to decode" >> {
    "when the type is not valid" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("foo"))).leftMap(_.message) must beLeft(
        "foo is not well formed type")
    }
    "when it is not valid schema" >> {
      decoder.decodeJson(Json.fromString("string")).leftMap(_.message) must beLeft(
        "Attempt to decode value on failed cursor")
    }

    "when array does not have values" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("array"))).leftMap(_.message) must beLeft(
        "array is not well formed type")
    }
    "when object does not have properties" >> {
      decoder
        .decodeJson(Json.obj("type" -> Json.fromString("object"), "required" -> Json.arr(Json.fromString("foo"))))
        .leftMap(_.message) must beLeft("object is not well formed type")
    }
  }
}
