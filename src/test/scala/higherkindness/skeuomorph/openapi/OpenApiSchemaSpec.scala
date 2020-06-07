/*
 * Copyright 2018-2020 47 Degrees Open Source <https://www.47deg.com>
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
import _root_.cats.implicits._
import org.specs2.{ScalaCheck, Specification}
import schema._
import org.scalacheck._
import _root_.io.circe._
import _root_.io.circe.testing._
import _root_.cats.kernel.CommutativeMonoid
import _root_.cats.laws.discipline.arbitrary._
import Spec._

class OpenApiSchemaSpec extends Specification with ScalaCheck with Discipline {

  implicit val miniIntMultiplication: CommutativeMonoid[MiniInt] = MiniInt.miniIntMultiplication

  def is = s2"""
      $shouldAbleCodecRoundTrip
      $shouldAbleToReadSpecExamples
      $functor
      $foldable
      $traverse
    """
  val traverse =
    checkAll(
      "Traverse[JsonSchemaF]",
      TraverseTests[JsonSchemaF].traverse[MiniInt, MiniInt, MiniInt, Set[MiniInt], Option, Option]
    )
  val functor  = checkAll("Functor[JsonSchemaF]", FunctorTests[JsonSchemaF].functor[MiniInt, MiniInt, String])
  val foldable = checkAll("Foldable[JsonSchemaF]", FoldableTests[JsonSchemaF].foldable[MiniInt, MiniInt])

  def shouldAbleCodecRoundTrip =
    Prop.forAll { (openApi: OpenApi[JsonSchemaF.Fixed]) =>
      import JsonEncoders._
      import JsonDecoders._

      CodecTests[OpenApi[JsonSchemaF.Fixed]].laws.codecRoundTrip(openApi)
    }

  def shouldAbleToReadSpecExamples =
    Prop.forAll { (format: Spec.Format) =>
      import JsonDecoders._
      import _root_.higherkindness.skeuomorph.openapi.yaml.{Decoder => YamlDecoder, _}
      format.fold(
        YamlDecoder[OpenApi[JsonSchemaF.Fixed]].apply(_).isRight,
        Decoder[OpenApi[JsonSchemaF.Fixed]].decodeJson(_).isRight
      )
    }
}

object Spec {
  type Yaml   = String
  type Format = Either[String, Json]

  private val examples =
    List("api-with-examples", "callback-example", "link-example", "petstore-expanded", "petstore", "uspto")

  private def fromResource(fileName: String): String =
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(fileName))
      .getLines()
      .toList
      .mkString("\n")

  private def examplesArbitrary[A](f: String => String)(g: String => A): Gen[A] =
    Gen.oneOf(
      examples
        .map(x => g(fromResource(f(x))))
    )

  implicit val yamlArbitrary: Arbitrary[Format] =
    Arbitrary(
      eitherGen(
        examplesArbitrary(x => s"/openapi/yaml/$x.yaml")(identity),
        examplesArbitrary(x => s"/openapi/json/$x.json")(helpers.unsafeParse)
      )
    )

}
