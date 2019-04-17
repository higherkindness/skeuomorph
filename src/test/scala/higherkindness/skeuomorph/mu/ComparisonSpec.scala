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

import qq.droste.data.Mu
import qq.droste.syntax.embed._
import org.specs2.Specification
import cats.data.NonEmptyList

import MuF._
import ComparisonResult._
import Transformation._
import PathElement._

class ComparisonSpec extends Specification {

  def is =
    s2"""
  Schema comparison

  Should accept numeric widdening $numericWiddening
  Should accept coproduct creation $coproductCreation
  Should accept coproduct widenning $coproductWiddening
  Should accept field addition in records $fieldAddition
  Should accept field removal in records $fieldRemoval
  """

  type T = Mu[MuF]

  def numericWiddening = {

    val validWiddenings = List(
      int[T].embed   -> List(long[T].embed, float[T].embed, double[T].embed),
      long[T].embed  -> List(float[T].embed, double[T].embed),
      float[T].embed -> List(double[T].embed)
    )

    for {
      (w, rs) <- validWiddenings
      r       <- rs
    } yield Comparison(w, r) must_== Match(List(NumericWiddening(Path.empty, w, r)))
  }

  def coproductCreation = {
    val original = int[T].embed
    val extended = coproduct(NonEmptyList.of(string[T].embed, long[T].embed)).embed

    Comparison(original, extended) must_== Match(List())
  }

  def coproductWiddening = {
    val original = coproduct(NonEmptyList.of(string[T].embed, long[T].embed)).embed
    val extended = coproduct(NonEmptyList.of(long[T].embed, byteArray[T].embed, boolean[T].embed)).embed

    Comparison(original, extended) must_== Match(List())
  }

  def fieldAddition = {
    val original = product("foo", List(Field("name", string[T].embed))).embed
    val extended = product("foo", List(Field("name", string[T].embed), Field("age", int[T].embed))).embed

    Comparison(original, extended) must_== Match(
      List(Addition(Path.empty / Name("foo") / FieldName("age"), int[T].embed)))
  }

  def fieldRemoval = {
    val original = product("foo", List(Field("name", string[T].embed), Field("age", int[T].embed))).embed
    val reduced  = product("foo", List(Field("name", string[T].embed))).embed

    Comparison(original, reduced) must_== Match(
      List(Removal(Path.empty / Name("foo") / FieldName("age"), int[T].embed)))
  }
}
