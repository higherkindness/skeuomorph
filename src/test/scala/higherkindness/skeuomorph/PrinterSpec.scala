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

import org.typelevel.discipline.specs2.Discipline
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.ContravariantMonoidalTests
import cats.laws.discipline.eq._

import cats.ContravariantSemigroupal
import cats.ContravariantMonoidal
import cats.kernel.Eq
import cats.instances.string._

import org.specs2._
import org.scalacheck._

class PrinterSpec extends Specification with ScalaCheck with Discipline {

  implicit def printerArbitrary[T: Cogen]: Arbitrary[Printer[T]] =
    Arbitrary(implicitly[Arbitrary[T => String]].arbitrary.map(Printer.apply))

  implicit def printerEq[T: Arbitrary]: Eq[Printer[T]] =
    Eq.by[Printer[T], T => String](_.print)
  implicit val printerMonoid    = ContravariantMonoidal.monoid[Printer, Int]
  implicit val printerSemigroup = ContravariantSemigroupal.semigroup[Printer, Int]

  def is = s2"""
  $contravariantMonoidalPrinter


  $monoidPrinter


  $semigroupPrinter
  """

  val contravariantMonoidalPrinter = checkAll(
    "ContravariantMonoidal[Printer].contravariantMonoidal",
    ContravariantMonoidalTests[Printer].contravariantMonoidal[Int, Int, Int])

  val monoidPrinter    = checkAll("ContravariantMonoidal[Printer].monoid", MonoidTests[Printer[Int]].monoid)
  val semigroupPrinter = checkAll("ContravariantSemigroupal[Printer].semigroup", SemigroupTests[Printer[Int]].semigroup)

}
