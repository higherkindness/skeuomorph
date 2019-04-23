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

package higherkindness.skeuomorph.mu.comparison

import cats.Show
import cats.Monoid
import cats.data.NonEmptyList
import cats.syntax.show._

sealed trait Incompatibility

object Incompatibility {
  final case class Different(relativePath: Path)          extends Incompatibility
  final case class UnionMemberRemoved(relativePath: Path) extends Incompatibility

  implicit val incompatibilityShow: Show[Incompatibility] = Show.show {
    case Different(p)          => p.show ++ ": !!DIFFERENT!!"
    case UnionMemberRemoved(p) => p.show ++ ": union member not found in reader schema"
  }
}

sealed trait ComparisonResult[T] {
  def transformations: List[Transformation[T]]
}
object ComparisonResult {

  final case class Match[T](transformations: List[Transformation[T]]) extends ComparisonResult[T]
  final case class Mismatch[T](transformations: List[Transformation[T]], discrepancies: NonEmptyList[Incompatibility])
      extends ComparisonResult[T]

  def isMatch[T](result: ComparisonResult[T]): Boolean = result match {
    case Match(_)       => true
    case Mismatch(_, _) => false
  }

  implicit def comparisonResultCatsMonoid[T]: Monoid[ComparisonResult[T]] =
    new Monoid[ComparisonResult[T]] {
      def empty = Match[T](Nil)
      def combine(left: ComparisonResult[T], right: ComparisonResult[T]): ComparisonResult[T] = (left, right) match {
        case (Match(t1), Match(t2))               => Match(t1 ++ t2)
        case (Match(t1), Mismatch(t2, d2))        => Mismatch(t1 ++ t2, d2)
        case (Mismatch(t1, d1), Match(t2))        => Mismatch(t1 ++ t2, d1)
        case (Mismatch(t1, d1), Mismatch(t2, d2)) => Mismatch(t1 ++ t2, d1 ++ d2.toList)
      }
    }

  implicit def comparisonResultShow[T](
      implicit showTrans: Show[Transformation[T]],
      showIncomp: Show[Incompatibility]): Show[ComparisonResult[T]] = Show.show {
    case Match(Nil)     => "schemas are identical"
    case Match(t)       => "compatible transformations detected:\n" ++ t.map(_.show).mkString("\n")
    case Mismatch(_, i) => "schemas are incompatible:\n" ++ i.toList.map(_.show).mkString("\n")
  }

}
