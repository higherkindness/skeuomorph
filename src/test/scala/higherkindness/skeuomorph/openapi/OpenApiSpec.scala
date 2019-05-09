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

import higherkindness.skeuomorph.instances._
import org.typelevel.discipline.specs2.Discipline
import cats.laws.discipline._
import cats.implicits._
import org.specs2._
import schema._

import org.scalacheck._
class OpenApiSchemaSpec extends Specification with ScalaCheck with Discipline {

  def is = s2"""
      $functor
      $foldable
      $traverse
      $shouldAbleToEncodeAndDecode
    """
  val traverse =
    checkAll("Traverse[JsonSchemaF]", TraverseTests[JsonSchemaF].traverse[Int, Int, Int, Set[Int], Option, Option])
  val functor  = checkAll("Functor[JsonSchemaF]", FunctorTests[JsonSchemaF].functor[Int, Int, String])
  val foldable = checkAll("Foldable[JsonSchemaF]", FoldableTests[JsonSchemaF].foldable[Int, Int])

  def shouldAbleToEncodeAndDecode = Prop.forAll { (openApi: OpenApi[Int]) =>
    import _root_.io.circe._
    import _root_.io.circe.syntax._
    import JsonEncoders._
    import JsonDecoders._
    Decoder[OpenApi[Int]].decodeJson(openApi.asJson) == (openApi.asRight)
  }
}
