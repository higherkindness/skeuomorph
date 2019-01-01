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

import cats.Functor
import io.circe.Json
import qq.droste._
import qq.droste.data.Fix

sealed trait JsonSchemaF[A]
object JsonSchemaF {
  final case class Property[A](name: String, tpe: A)

  final case class IntegerF[A]()  extends JsonSchemaF[A]
  final case class LongF[A]()     extends JsonSchemaF[A]
  final case class FloatF[A]()    extends JsonSchemaF[A]
  final case class DoubleF[A]()   extends JsonSchemaF[A]
  final case class StringF[A]()   extends JsonSchemaF[A]
  final case class ByteF[A]()     extends JsonSchemaF[A]
  final case class BinaryF[A]()   extends JsonSchemaF[A]
  final case class BooleanF[A]()  extends JsonSchemaF[A]
  final case class DateF[A]()     extends JsonSchemaF[A]
  final case class DateTimeF[A]() extends JsonSchemaF[A]
  final case class PasswordF[A]() extends JsonSchemaF[A]
  final case class ObjectF[A](name: String, properties: List[Property[A]], required: List[String])
      extends JsonSchemaF[A]
  final case class ArrayF[A](values: A)     extends JsonSchemaF[A]
  final case class EnumF[A](cases: List[A]) extends JsonSchemaF[A]

  implicit val jsonSchemaFunctor: Functor[JsonSchemaF] = new Functor[JsonSchemaF] {
    def map[A, B](fa: JsonSchemaF[A])(f: A => B): JsonSchemaF[B] = fa match {
      case IntegerF()  => IntegerF()
      case LongF()     => LongF()
      case FloatF()    => FloatF()
      case DoubleF()   => DoubleF()
      case StringF()   => StringF()
      case ByteF()     => ByteF()
      case BinaryF()   => BinaryF()
      case BooleanF()  => BooleanF()
      case DateF()     => DateF()
      case DateTimeF() => DateTimeF()
      case PasswordF() => PasswordF()
      case ObjectF(name, properties, required) =>
        ObjectF(name, properties.map(prop => Property(prop.name, f(prop.tpe))), required)
      case ArrayF(values) => ArrayF(f(values))
      case EnumF(cases)   => EnumF(cases.map(f))
    }
  }

  def render: Algebra[JsonSchemaF, Json] = Algebra {
    case IntegerF()  => Json.fromString("integer")
    case LongF()     => Json.fromString("long")
    case FloatF()    => Json.fromString("float")
    case DoubleF()   => Json.fromString("double")
    case StringF()   => Json.fromString("string")
    case ByteF()     => Json.fromString("byte")
    case BinaryF()   => Json.fromString("binary")
    case BooleanF()  => Json.fromString("boolean")
    case DateF()     => Json.fromString("date")
    case DateTimeF() => Json.fromString("datetime")
    case PasswordF() => Json.fromString("password")
    case ObjectF(name, properties, required) =>
      Json.obj(
        name -> Json.obj(
          "type"       -> Json.fromString("object"),
          "properties" -> Json.obj(properties.map(prop => prop.name -> prop.tpe): _*),
          "required"   -> Json.fromValues(required.map(Json.fromString))
        )
      )
    case ArrayF(values) =>
      Json.obj(
        "type"  -> Json.fromString("array"),
        "items" -> Json.obj("type" -> values)
      )
    case EnumF(cases) =>
      Json.obj(
        "type" -> Json.fromString("string"),
        "enum" -> Json.fromValues(cases)
      )

  }

  def addressSchema: Fix[JsonSchemaF] =
    Fix(
      ObjectF(
        "address",
        List(
          Property("street_address", Fix(StringF())),
          Property("city", Fix(StringF())),
          Property("state", Fix(StringF()))
        ),
        List(
          "street_address",
          "city",
          "state"
        )
      ))
}
