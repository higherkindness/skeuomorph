/*
 * Copyright 2017-2018 47 Degrees, LLC. <http://www.47deg.com>
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
package freestyle

import cats.Functor
import cats.data.NonEmptyList

sealed trait Schema[A]
object Schema {
  case class Field[A](name: String, tpe: A)

  case class TNull[A]()                                        extends Schema[A]
  case class TDouble[A]()                                      extends Schema[A]
  case class TFloat[A]()                                       extends Schema[A]
  case class TInt[A]()                                         extends Schema[A]
  case class TLong[A]()                                        extends Schema[A]
  case class TBoolean[A]()                                     extends Schema[A]
  case class TString[A]()                                      extends Schema[A]
  case class TByteArray[A]()                                   extends Schema[A]
  case class TNamedType[A](name: String)                       extends Schema[A]
  case class TOption[A](value: A)                              extends Schema[A]
  case class TList[A](value: A)                                extends Schema[A]
  case class TMap[A](value: A)                                 extends Schema[A]
  case class TRequired[A](value: A)                            extends Schema[A]
  case class TCoproduct[A](invariants: NonEmptyList[A])        extends Schema[A]
  case class TSum[A](name: String, fields: List[String])       extends Schema[A]
  case class TProduct[A](name: String, fields: List[Field[A]]) extends Schema[A]

  implicit val schemaFunctor: Functor[Schema] = new Functor[Schema] {
    def map[A, B](fa: Schema[A])(f: A => B): Schema[B] = fa match {
      case TNull()                => TNull()
      case TDouble()              => TDouble()
      case TFloat()               => TFloat()
      case TInt()                 => TInt()
      case TLong()                => TLong()
      case TBoolean()             => TBoolean()
      case TString()              => TString()
      case TByteArray()           => TByteArray()
      case TNamedType(name)       => TNamedType(name)
      case TOption(value)         => TOption(f(value))
      case TList(value)           => TList(f(value))
      case TMap(value)            => TMap(f(value))
      case TRequired(value)       => TRequired(f(value))
      case TCoproduct(invariants) => TCoproduct(invariants.map(f))
      case TSum(name, fields)     => TSum(name, fields)
      case TProduct(name, fields) => TProduct(name, fields.map(field => field.copy(tpe = f(field.tpe))))
    }
  }

}
