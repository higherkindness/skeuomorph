/*
 * Copyright 2018-2020 47 Degrees, LLC. <http://www.47deg.com>
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

import cats.Functor

trait ComparisonInstances {

  implicit def comparisonCatsFunctor[T] = new Functor[Comparison[T, ?]] {
    def map[A, B](fa: Comparison[T, A])(f: (A) => B): Comparison[T, B] = fa match {
      case Comparison.End(res)                  => Comparison.End(res)
      case Comparison.Compare(a, rep)           => Comparison.Compare(f(a), rep)
      case Comparison.CompareBoth(x, y)         => Comparison.CompareBoth(f(x), f(y))
      case Comparison.CompareList(i, rep)       => Comparison.CompareList(i.map(f), rep)
      case Comparison.MatchInList(a, rep)       => Comparison.MatchInList(a.map(f), rep)
      case Comparison.AlignUnionMembers(a, rep) => Comparison.AlignUnionMembers(a.mapValues(_.map(f)), rep)
    }
  }
}
