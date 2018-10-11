/*
 * Copyright 2018 47 Degrees, LLC. <http://www.47deg.com>
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

/**
 *
 */
sealed trait FreesF[A]
object FreesF {
  case class Field[A](name: String, tpe: A)

  case class TNull[A]()                                        extends FreesF[A]
  case class TDouble[A]()                                      extends FreesF[A]
  case class TFloat[A]()                                       extends FreesF[A]
  case class TInt[A]()                                         extends FreesF[A]
  case class TLong[A]()                                        extends FreesF[A]
  case class TBoolean[A]()                                     extends FreesF[A]
  case class TString[A]()                                      extends FreesF[A]
  case class TByteArray[A]()                                   extends FreesF[A]
  case class TNamedType[A](name: String)                       extends FreesF[A]
  case class TOption[A](value: A)                              extends FreesF[A]
  case class TList[A](value: A)                                extends FreesF[A]
  case class TMap[A](value: A)                                 extends FreesF[A]
  case class TGeneric[A](generic: A, params: List[A])          extends FreesF[A]
  case class TRequired[A](value: A)                            extends FreesF[A]
  case class TCoproduct[A](invariants: NonEmptyList[A])        extends FreesF[A]
  case class TSum[A](name: String, fields: List[String])       extends FreesF[A]
  case class TProduct[A](name: String, fields: List[Field[A]]) extends FreesF[A]

  implicit val freestyleFunctor: Functor[FreesF] = new Functor[FreesF] {
    def map[A, B](fa: FreesF[A])(f: A => B): FreesF[B] = fa match {
      case TNull()                   => TNull()
      case TDouble()                 => TDouble()
      case TFloat()                  => TFloat()
      case TInt()                    => TInt()
      case TLong()                   => TLong()
      case TBoolean()                => TBoolean()
      case TString()                 => TString()
      case TByteArray()              => TByteArray()
      case TNamedType(name)          => TNamedType(name)
      case TOption(value)            => TOption(f(value))
      case TList(value)              => TList(f(value))
      case TMap(value)               => TMap(f(value))
      case TGeneric(generic, params) => TGeneric(f(generic), params.map(f))
      case TRequired(value)          => TRequired(f(value))
      case TCoproduct(invariants)    => TCoproduct(invariants.map(f))
      case TSum(name, fields)        => TSum(name, fields)
      case TProduct(name, fields)    => TProduct(name, fields.map(field => field.copy(tpe = f(field.tpe))))
    }
  }
}
