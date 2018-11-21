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

package higherkindness.skeuomorph.mu

import qq.droste.macros.deriveTraverse

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.string._
import cats.syntax.eq._

/**
 *
 */
@deriveTraverse sealed trait MuF[A]
object MuF {
  @deriveTraverse final case class Field[A](name: String, tpe: A)

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
  final case class TMap[A](value: A)                                 extends MuF[A]
  final case class TGeneric[A](generic: A, params: List[A])          extends MuF[A]
  final case class TRequired[A](value: A)                            extends MuF[A]
  final case class TCoproduct[A](invariants: NonEmptyList[A])        extends MuF[A]
  final case class TSum[A](name: String, fields: List[String])       extends MuF[A]
  final case class TProduct[A](name: String, fields: List[Field[A]]) extends MuF[A]

  implicit def fieldEq[T](implicit T: Eq[T]): Eq[Field[T]] = Eq.instance {
    case (Field(n, t), Field(n2, t2)) => n === n2 && t === t2
  }

  implicit def muEq[T](implicit T: Eq[T]): Eq[MuF[T]] = Eq.instance {
    case (TNull(), TNull())           => true
    case (TDouble(), TDouble())       => true
    case (TFloat(), TFloat())         => true
    case (TInt(), TInt())             => true
    case (TLong(), TLong())           => true
    case (TBoolean(), TBoolean())     => true
    case (TString(), TString())       => true
    case (TByteArray(), TByteArray()) => true

    case (TNamedType(a), TNamedType(b)) => a === b
    case (TOption(a), TOption(b))       => a === b
    case (TList(a), TList(b))           => a === b
    case (TMap(a), TMap(b))             => a === b
    case (TRequired(a), TRequired(b))   => a === b

    case (TEither(l, r), TEither(l2, r2))   => l === l2 && r === r2
    case (TGeneric(g, p), TGeneric(g2, p2)) => g === g2 && p === p2
    case (TCoproduct(i), TCoproduct(i2))    => i === i2
    case (TSum(n, f), TSum(n2, f2))         => n === n2 && f === f2
    case (TProduct(n, f), TProduct(n2, f2)) => n === n2 && f === f2

    case _ => false
  }

  // smart constructors, to avoid scala inferring specific types instead of MuF
  def `null`[A](): MuF[A]                                      = TNull()
  def double[A](): MuF[A]                                      = TDouble()
  def float[A](): MuF[A]                                       = TFloat()
  def int[A](): MuF[A]                                         = TInt()
  def long[A](): MuF[A]                                        = TLong()
  def boolean[A](): MuF[A]                                     = TBoolean()
  def string[A](): MuF[A]                                      = TString()
  def byteArray[A](): MuF[A]                                   = TByteArray()
  def namedType[A](name: String): MuF[A]                       = TNamedType(name)
  def option[A](value: A): MuF[A]                              = TOption(value)
  def either[A](left: A, right: A): MuF[A]                     = TEither(left, right)
  def list[A](value: A): MuF[A]                                = TList(value)
  def map[A](value: A): MuF[A]                                 = TMap(value)
  def generic[A](generic: A, params: List[A]): MuF[A]          = TGeneric(generic, params)
  def required[A](value: A): MuF[A]                            = TRequired(value)
  def coproduct[A](invariants: NonEmptyList[A]): MuF[A]        = TCoproduct(invariants)
  def sum[A](name: String, fields: List[String]): MuF[A]       = TSum(name, fields)
  def product[A](name: String, fields: List[Field[A]]): MuF[A] = TProduct(name, fields)
}
