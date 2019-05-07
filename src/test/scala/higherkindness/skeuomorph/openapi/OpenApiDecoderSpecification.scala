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
import io.circe.parser._
import schema._
import cats.implicits._
import io.circe.Json
import JsonSchemaF.Fixed

class OpenApiDecoderSpecification extends org.specs2.mutable.Specification {
  def unsafeParse: String => Json = parse(_).valueOr(x => sys.error(x.message))

  "Info object should be able to decode" >> {
    "when is valid" >> {
      val json = unsafeParse("""{
            "title": "Sample Pet Store App",
            "description": "This is a sample server for a pet store.",
            "termsOfService": "http://example.com/terms/",
            "contact": {
              "name": "API Support",
              "url": "http://www.example.com/support",
              "email": "support@example.com"
            },
            "license": {
              "name": "Apache 2.0",
              "url": "https://www.apache.org/licenses/LICENSE-2.0.html"
            },
            "version": "1.0.1"
          }""")

      Decoder[Info].decodeJson(json) must beRight(
        Info("Sample Pet Store App", "This is a sample server for a pet store.".some, "1.0.1"))
    }
  }

  "Header object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("""
      {
        "description": "The number of allowed requests in the current period",
        "schema": {
          "type": "integer"
        }
      }
      """)
      Decoder[Header[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Header[JsonSchemaF.Fixed]("The number of allowed requests in the current period", Fixed.integer()))
    }

  }

  "Tag object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("""
      {
        "name": "pet",
        "description": "Pets operations"
      }
      """)
      Decoder[Tag].decodeJson(json) must beRight(Tag("pet", "Pets operations".some, None))
    }

  }

  "External Documentation object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("""
      {
        "description": "Find more info here",
        "url": "https://example.com"
      }
      """)
      Decoder[ExternalDocs].decodeJson(json) must beRight(
        ExternalDocs("https://example.com", "Find more info here".some))
    }

  }

  "Reference object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse(s"""
      {
        "$$ref": "#/components/schemas/Pet"
      }
      """)

      Decoder[Reference].decodeJson(json) must beRight(Reference("#/components/schemas/Pet"))
    }
  }

  "Components object should be able to decode" >> {
    "when an empty object is provided" >> {
      val json = unsafeParse("{}")
      Decoder[Components[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Components[JsonSchemaF.Fixed](
          Map.empty,
          Map.empty
        )
      )
    }
  }

  "Encoding object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("{}")
      Decoder[Encoding[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Encoding[JsonSchemaF.Fixed](None, Map.empty, None, None, None))
    }
  }

  "Server object should be able to decode" >> {
    "when is valid and variables are not provided" >> {
      val json = unsafeParse(
        """{
          "url": "https://development.gigantic-server.com/v1",
          "description": "Development server"
        }"""
      )

      Decoder[Server].decodeJson(json) must beRight(
        Server("https://development.gigantic-server.com/v1", "Development server".some, Map.empty))
    }

    "when is valid and variables are provided" >> {
      val json = unsafeParse(
        """{
          "url": "https://{username}.gigantic-server.com:{port}/{basePath}",
          "description": "The production API server",
          "variables": {
            "username": {
              "default": "demo",
              "description": "this value is assigned by the service provider, in this example `gigantic-server.com`"
            },
            "port": {
              "enum": [
                "8443",
                "443"
              ],
              "default": "8443"
            },
            "basePath": {
              "default": "v2"
            }
          }
        }"""
      )

      Decoder[Server].decodeJson(json) must beRight(
        Server(
          "https://{username}.gigantic-server.com:{port}/{basePath}",
          "The production API server".some,
          Map(
            "username" -> Server.Variable(
              List.empty,
              "demo",
              """this value is assigned by the service provider, in this example `gigantic-server.com`""".some),
            "port"     -> Server.Variable(List("8443", "443"), "8443", None),
            "basePath" -> Server.Variable(List.empty, "v2", None)
          )
        ))
    }
  }
  "Open api object should be able to decode" >> {
    "when required fields are provided" >> {
      val json = unsafeParse("""
      {
        "openapi" : "3.0.0",
        "info" : {
          "title": "Swagger Petstore",
          "version": "1.0.0"
        },
        "paths": {
          "/pets": {
          }
        }
      }
      """)

      Decoder[OpenApi[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        OpenApi[JsonSchemaF.Fixed](
          "3.0.0",
          Info("Swagger Petstore", None, "1.0.0"),
          List.empty,
          Map(
            "/pets" -> Path.ItemObject[JsonSchemaF.Fixed](
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
            )),
          None,
          List.empty,
          None
        ))
    }
  }

  "Path item object should be able to decode" >> {
    "when only parameters are provided" >> {
      val json = unsafeParse("""{
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "description": "ID of pet to use",
            "required": true,
            "schema": {
              "type": "array",
              "items": {
                "type": "string"
              }
            },
            "style": "simple"
          }
        ]
      }""")

      Decoder[Path.ItemObject[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Path.ItemObject[JsonSchemaF.Fixed](
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
        ))
    }

    "when get operation is defined" >> {
      val json         = unsafeParse("""
      {
        "post": 
      {
        "tags": [
          "pet"
        ],
        "summary": "Updates a pet in the store with form data",
        "operationId": "updatePetWithForm",
        "parameters": [
          {
            "name": "petId",
            "in": "path",
            "description": "ID of pet that needs to be updated",
            "required": true,
            "schema": {
              "type": "string"
            }
          }
        ],
        "requestBody": {
          "content": {
            "application/x-www-form-urlencoded": {
              "schema": {
                "type": "object",
                 "properties": {
                    "name": { 
                      "description": "Updated name of the pet",
                      "type": "string"
                    },
                    "status": {
                      "description": "Updated status of the pet",
                      "type": "string"
                   }
                 },
              "required": ["status"] 
              }
            }
          }
        },
        "responses": {
          "200": {
            "description": "Pet updated.",
            "content": {
              "application/json": {},
              "application/xml": {}
            }
          },
          "405": {
            "description": "Method Not Allowed",
            "content": {
              "application/json": {},
              "application/xml": {}
            }
          }
        },
        "security": [
          {
            "petstore_auth": [
              "write:pets",
              "read:pets"
            ]
          }
        ]
      }
    }
      """)
      val nonMediaType = MediaType[JsonSchemaF.Fixed](None, Map.empty)

      val expectedResponseContent = Map(
        "application/json" -> nonMediaType,
        "application/xml"  -> nonMediaType
      )

      val expectedResponses = Map(
        "200" -> Response("Pet updated.", Map.empty, expectedResponseContent).asLeft,
        "405" -> Response("Method Not Allowed", Map.empty, expectedResponseContent).asLeft
      )
      val expectedRequest = Map(
        "application/x-www-form-urlencoded" ->
          MediaType[JsonSchemaF.Fixed](
            Fixed
              .`object`(
                List(
                  "name"   -> Fixed.string(),
                  "status" -> Fixed.string()
                ),
                List("status")
              )
              .some,
            Map.empty)
      )

      val expectedOperation =
        Path.Operation[JsonSchemaF.Fixed](
          List("pet"),
          "Updates a pet in the store with form data".some,
          None,
          None,
          "updatePetWithForm".some,
          List(
            Parameter
              .Path(
                name = "petId",
                description = "ID of pet that needs to be updated".some,
                schema = JsonSchemaF.Fixed.string())
              .asLeft),
          Request(description = None, content = expectedRequest, false).asLeft,
          expectedResponses,
          false,
          List.empty
        )
      Decoder[Path.ItemObject[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Path.ItemObject[JsonSchemaF.Fixed](
          ref = None,
          summary = None,
          description = None,
          get = None,
          put = None,
          post = expectedOperation.some,
          delete = None,
          options = None,
          head = None,
          patch = None,
          trace = None,
          List.empty
        ))
    }

  }
}
