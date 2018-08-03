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

import qq.droste._
import FreesF._

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
   * case class Product(field1: String, field2: case class OtherField())
   * }}}
   *
   * With it, we cut recursion in messages, to leave only type names:
   *
   * {{{
   * case class Product(field1: String, field2: OtherField)
   * }}}
   */
  def nestedNamedTypesTrans[T](implicit T: Basis[FreesF, T]): Trans[FreesF, FreesF, T] = Trans {
    case TProduct(name, fields) =>
      def nameTypes(f: Field[T]): Field[T] = f.copy(tpe = namedTypes(T)(f.tpe))
      TProduct[T](
        name,
        fields.map(nameTypes)
      )
    case other => other
  }

  def namedTypesTrans[T]: Trans[FreesF, FreesF, T] = Trans {
    case TProduct(name, _) => TNamedType[T](name)
    case TSum(name, _)     => TNamedType[T](name)
    case other             => other
  }

  def namedTypes[T: Basis[FreesF, ?]]: T => T       = scheme.cata(namedTypesTrans.algebra)
  def nestedNamedTypes[T: Basis[FreesF, ?]]: T => T = scheme.cata(nestedNamedTypesTrans.algebra)
}
