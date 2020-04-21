/*
 * Copyright 2018-2020 47 Degrees <http://47deg.com>
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

import cats.{ContravariantMonoidal, ContravariantSemigroupal}
import cats.instances.string._
import cats.kernel.Eq
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ContravariantMonoidalTests, ExhaustiveCheck, MiniInt}
import org.scalacheck.{Arbitrary, Cogen}
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

class PrinterSpec extends Specification with ScalaCheck with Discipline {

  implicit def printerArbitrary[T: Cogen]: Arbitrary[Printer[T]] =
    Arbitrary(implicitly[Arbitrary[T => String]].arbitrary.map(Printer.print))

  implicit def catsLawsEqForFn1Exhaustive[A, B](implicit A: ExhaustiveCheck[A], B: Eq[B]): Eq[A => B] =
    Eq.instance((f, g) => A.allValues.forall(a => B.eqv(f(a), g(a))))

  implicit val printerMonoid    = ContravariantMonoidal.monoid[Printer, MiniInt]
  implicit val printerSemigroup = ContravariantSemigroupal.semigroup[Printer, MiniInt]

  def is = s2"""
  $contravariantMonoidalPrinter


  $monoidPrinter


  $semigroupPrinter
  """

  val contravariantMonoidalPrinter = checkAll(
    "ContravariantMonoidal[Printer].contravariantMonoidal",
    ContravariantMonoidalTests[Printer].contravariantMonoidal[MiniInt, MiniInt, MiniInt]
  )

  val monoidPrinter = checkAll("ContravariantMonoidal[Printer].monoid", MonoidTests[Printer[MiniInt]].monoid)
  val semigroupPrinter =
    checkAll("ContravariantSemigroupal[Printer].semigroup", SemigroupTests[Printer[MiniInt]].semigroup)

}
