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

package higherkindness.skeuomorph.mu

import cats.Functor
import cats.data.NonEmptyList

/**
 *
 */
sealed trait MuF[A]
object MuF {
  final case class Field[A](name: String, tpe: A)

  final case class TNull[A]()                                        extends MuF[A]
  final case class TDouble[A]()                                      extends MuF[A]
  final case class TFloat[A]()                                       extends MuF[A]
  final case class TInt[A]()                                         extends MuF[A]
  final case class TLong[A]()                                        extends MuF[A]
  final case class TBoolean[A]()                                     extends MuF[A]
  final case class TString[A]()                                      extends MuF[A]
  final case class TByteArray[A]()                                   extends MuF[A]
  final case class TNamedType[A](name: String)                       extends MuF[A]
  final case class TOption[A](value: A)                              extends MuF[A]
  final case class TEither[A](left: A, right: A)                     extends MuF[A]
  final case class TList[A](value: A)                                extends MuF[A]
  final case class TMap[A](keyTpe: Option[A], value: A)              extends MuF[A]
  final case class TGeneric[A](generic: A, params: List[A])          extends MuF[A]
  final case class TContaining[A](values: List[A])                   extends MuF[A]
  final case class TRequired[A](value: A)                            extends MuF[A]
  final case class TCoproduct[A](invariants: NonEmptyList[A])        extends MuF[A]
  final case class TSum[A](name: String, fields: List[String])       extends MuF[A]
  final case class TProduct[A](name: String, fields: List[Field[A]]) extends MuF[A]

  implicit val muFunctor: Functor[MuF] = new Functor[MuF] {
    def map[A, B](fa: MuF[A])(f: A => B): MuF[B] = fa match {
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
      case TEither(left, right)      => TEither(f(left), f(right))
      case TList(value)              => TList(f(value))
      case TMap(key, value)          => TMap(key.map(f), f(value))
      case TGeneric(generic, params) => TGeneric(f(generic), params.map(f))
      case TContaining(values)       => TContaining(values.map(f))
      case TRequired(value)          => TRequired(f(value))
      case TCoproduct(invariants)    => TCoproduct(invariants.map(f))
      case TSum(name, fields)        => TSum(name, fields)
      case TProduct(name, fields)    => TProduct(name, fields.map(field => field.copy(tpe = f(field.tpe))))
    }
  }
}
