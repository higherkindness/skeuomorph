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

package higherkindness.skeuomorph
package mu

import cats.Functor
import cats.data.NonEmptyList
import higherkindness.droste._
import uast._
import uast.types._

/**
 * Optimize object contains transformations in same schema
 */
object Optimize {

  /**
   * micro-optimization to convert types from fields in a product to
   * NamedTypes.
   *
   * Without this optimization, printing a product containing fields
   * of other products would end up with something like:
   *
   * {{{
   * case class bbProduct(field1: String, field2: case class OtherField())
   * }}}
   *
   * With it, we cut recursion in messages, to leave only type names:
   *
   * {{{
   * case class Product(field1: String, field2: OtherField)
   * }}}
   */
  def nestedNamedTypesTrans[F[α] <: ACopK[α], T](
      implicit
      P: TRecord :<<: F,
      E: TEnum :<<: F,
      N: TNamedType :<<: F,
      F: Functor[F],
      T: Basis[F, T],
  ): Trans[F, F, T] = Trans {
    case P(TRecord(name, fields)) =>
      val nameTypes: FieldF[T] => FieldF[T] = f => FieldF.fieldType.modify(namedTypes[F, T].apply)(f)
      P.inj(
        TRecord[T](
          name,
          fields.map(nameTypes)
        ))
    case other => other
  }

  // def nestedNamedTypes[F[α] <: ACopK[α]: Functor, T: Basis[F, ?]](
  //     implicit
  //     P: TRecord :<<: F,
  //     S: TEnum :<<: F,
  //     N: TNamedType :<<: F
  // ): T => T = scheme.cata(nestedNamedTypesTrans[F, T].algebra)

  def namedTypesTrans[
      F[α] <: ACopK[α],
      T
  ](
      implicit
      P: TRecord :<<: F,
      S: TEnum :<<: F,
      N: TNamedType :<<: F
  ): Trans[F, F, T] = Trans {
    case P(TRecord(name, _)) => namedType[F, T](name)
    case S(TEnum(name, _))   => namedType[F, T](name)
    case other               => other
  }

  def namedTypes[F[α] <: ACopK[α]: Functor, T: Basis[F, ?]](
      implicit
      P: TRecord :<<: F,
      S: TEnum :<<: F,
      N: TNamedType :<<: F
  ): T => T = scheme.cata(namedTypesTrans[F, T].algebra)

  /**
   * micro-optimization to convert known coproducts to named types
   * such as Option or Either.
   *
   * Without this optimization, printing a product containing fields
   * whose type is a coproduct would end up with something like:
   *
   * {{{
   * case class Product(field1: Cop[Int :: String :: TNil], field2: Cop[Int :: Null :: TNil])
   * }}}
   *
   * With it, we rename the known coproducts to the correspondent named types:
   *
   * {{{
   * case class Product(field1: Either[Int, String], field2: Option[Int])
   * }}}
   */
  def knownUnionTypesTrans[F[α] <: ACopK[α], T](
      implicit
      U: TUnion :<<: F,
      O: TOption :<<: F,
      E: TEither :<<: F,
      N: TNull :<<: F,
      B: Basis[F, T]
  ): Trans[F, F, T] = Trans {
    case U(TUnion(NonEmptyList(x, List(y)))) =>
      (B.coalgebra(x), B.coalgebra(y)) match {
        case (_, N(_)) => O.inj(TOption[T](x))
        case (N(_), _) => O.inj(TOption[T](y))
        case _         => E.inj(TEither[T](x, y))
      }
    case other => other
  }

  def knownUnionTypes[T: Basis[mu.Type, ?]]: T => T = scheme.cata(knownUnionTypesTrans.algebra)
}
