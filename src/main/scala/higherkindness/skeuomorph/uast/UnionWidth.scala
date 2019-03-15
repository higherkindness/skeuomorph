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
package uast

import iota.{CopK, TListK, TNilK}
import iota.TListK.:::

/** Calculates the width of a typelevel union (coproduct). */
sealed abstract class UnionWidth[F[_]] {
  val width: Int
}

object UnionWidth extends UWidthInstances

sealed abstract class UWidthInstances extends UWidthInstances0 {

  implicit def copkUWidthInduct[F[_], LL <: TListK](
      implicit U: UnionWidth[CopK[LL, ?]]): UnionWidth[CopK[F ::: LL, ?]] =
    new UnionWidth[CopK[F ::: LL, ?]] { val width = U.width + 1 }

}

sealed abstract class UWidthInstances0 {
  implicit def defaultUWidth[F[_]]: UnionWidth[F] =
    new UnionWidth[F] { val width = 1 }

  implicit def copkUWidthBase[F[_]]: UnionWidth[CopK[F ::: TNilK, ?]] =
    new UnionWidth[CopK[F ::: TNilK, ?]] { val width = 1 }
}
