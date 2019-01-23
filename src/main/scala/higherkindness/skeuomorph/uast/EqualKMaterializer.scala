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

package higherkindness.skeuomorph.uast

import iota.TListK.:::
import iota.{CopK, TListK, TNilK}
import qq.droste.Delay

import cats._

/**
 * Taken from quasar's iota.contrib module
 *
 * https://github.com/slamdata/quasar/tree/7769fa346da0f16011d6476c057ed93efb09880a/foundation/src/main/scala/quasar/contrib/iota
 */
sealed trait EqKMaterializer[LL <: TListK] {
  def materialize(offset: Int): Delay[Eq, CopK[LL, ?]]
}

object EqKMaterializer {

  implicit def base[F[_]](
      implicit
      F: Delay[Eq, F]
  ): EqKMaterializer[F ::: TNilK] = new EqKMaterializer[F ::: TNilK] {
    override def materialize(offset: Int): Delay[Eq, CopK[F ::: TNilK, ?]] = {
      val I = mkInject[F, F ::: TNilK](offset)
      new (Eq ~> λ[a => Eq[CopK[F ::: TNilK, a]]]) {
        override def apply[A](eq: Eq[A]): Eq[CopK[F ::: TNilK, A]] = {
          Eq instance {
            case (I(left), I(right)) => F(eq).eqv(left, right)
            case _                   => false
          }
        }
      }
    }
  }

  implicit def induct[F[_], LL <: TListK](
      implicit
      F: Delay[Eq, F],
      LL: EqKMaterializer[LL]
  ): EqKMaterializer[F ::: LL] = new EqKMaterializer[F ::: LL] {
    override def materialize(offset: Int): Delay[Eq, CopK[F ::: LL, ?]] = {
      val I = mkInject[F, F ::: LL](offset)
      new (Eq ~> λ[a => Eq[CopK[F ::: LL, a]]]) {
        override def apply[A](eq: Eq[A]): Eq[CopK[F ::: LL, A]] = {
          Eq instance {
            case (I(left), I(right)) => F(eq).eqv(left, right)
            case (left, right) =>
              LL.materialize(offset + 1)(eq).eqv(left.asInstanceOf[CopK[LL, A]], right.asInstanceOf[CopK[LL, A]])
          }
        }
      }
    }
  }

}
