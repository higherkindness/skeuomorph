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

import org.scalacheck.{Gen, Prop}
import org.specs2.{ScalaCheck, Specification}

import cats.data.NonEmptyList

import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.uast.arbitraries._
import higherkindness.skeuomorph.instances._
import higherkindness.skeuomorph.mu.Optimize._

class OptimizeSpec extends Specification with ScalaCheck {

  def is =
    s2"""
  mu Optimize

  It should convert a TUnion into a TOption. $convertUnion2Option
  It should convert a TUnion into a TEither. $convertUnion2Either
  """

  val genPrimitiveType: Gen[Fix[Type]] = Gen
    .oneOf(
      arbitraryEmbedDelay[TDouble, Fix[TDouble]].arbitrary.map(x =>
        Fix(InjDouble.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]])),
      arbitraryEmbedDelay[TFloat, Fix[TFloat]].arbitrary.map(x =>
        Fix(InjFloat.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]])),
      arbitraryEmbedDelay[TInt, Fix[TInt]].arbitrary.map(x => Fix(InjInt.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]])),
      arbitraryEmbedDelay[TLong, Fix[TLong]].arbitrary.map(x =>
        Fix(InjLong.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]])),
      arbitraryEmbedDelay[TBoolean, Fix[TBoolean]].arbitrary.map(x =>
        Fix(InjBoolean.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]])),
      arbitraryEmbedDelay[TString, Fix[TString]].arbitrary.map(x =>
        Fix(InjString.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]])),
      arbitraryEmbedDelay[TByteArray, Fix[TByteArray]].arbitrary.map(x =>
        Fix(InjByteArray.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]])),
      arbitraryEmbedDelay[TByte, Fix[TByte]].arbitrary.map(x =>
        Fix(InjByte.inj(Fix.un(x)).asInstanceOf[Type[Fix[Type]]]))
    )

  val genTwoTypeUnionWithNull: Gen[TUnion[Fix[Type]]] = for {
    a <- delayArbitrary[TNull, Fix[Type]](arbitraryTNull, arbitraryEmbedDelay[Type, Fix[Type]]).arbitrary.map(x =>
      Fix(InjNull.inj(x)))
    b   <- genPrimitiveType
    tup <- Gen.oneOf((a, b), (b, a))
  } yield TUnion[Fix[Type]](NonEmptyList(tup._1, List(tup._2)))

  val genTwoTypeUnion: Gen[TUnion[Fix[Type]]] = for {
    a <- genPrimitiveType
    b <- genPrimitiveType
  } yield TUnion[Fix[Type]](NonEmptyList(a, List(b)))

  val convertUnion2Option: Prop = Prop.forAll(genTwoTypeUnionWithNull) { union: TUnion[Fix[Type]] =>
    val transformation: Fix[Type] = knownUnionTypesTrans[Type, Fix[Type]].algebra.run(InjUnion.inj(union))

    val test = scheme.hylo(checkOptionalValue, Project[Type, Fix[Type]].coalgebra)

    test(transformation)
  }

  val convertUnion2Either: Prop = Prop.forAll(genTwoTypeUnion) { union: TUnion[Fix[Type]] =>
    val transformation: Fix[Type] = knownUnionTypesTrans[Type, Fix[Type]].algebra.run(InjUnion.inj(union))

    val test = scheme.hylo(checkEitherValue, Project[Type, Fix[Type]].coalgebra)

    test(transformation)
  }

  val checkEitherValue: Algebra[Type, Boolean] = Algebra[Type, Boolean] {
    case InjEither(_) => true
    case _            => false
  }

  val checkOptionalValue: Algebra[Type, Boolean] = Algebra[Type, Boolean] {
    case InjOption(_) => true
    case _            => false
  }
}
