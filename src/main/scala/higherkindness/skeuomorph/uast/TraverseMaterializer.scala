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

import higherkindness.droste.util.DefaultTraverse

import iota.TListK.:::
import iota.{CopK, TListK, TNilK}

import cats.{Applicative, Traverse}

/**
 * Taken from quasar's iota.contrib module
 *
 * https://github.com/slamdata/quasar/tree/7769fa346da0f16011d6476c057ed93efb09880a/foundation/src/main/scala/quasar/contrib/iota
 */
sealed trait TraverseMaterializer[LL <: TListK] {
  def materialize(offset: Int): Traverse[CopK[LL, ?]]
}

object TraverseMaterializer {

  implicit def base[F[_]](
      implicit F: Traverse[F]
  ): TraverseMaterializer[F ::: TNilK] = new TraverseMaterializer[F ::: TNilK] {
    override def materialize(offset: Int): Traverse[CopK[F ::: TNilK, ?]] = {
      val I = mkInject[F, F ::: TNilK](offset)

      new DefaultTraverse[CopK[F ::: TNilK, ?]] {
        override def traverse[G[_], A, B](cfa: CopK[F ::: TNilK, A])(f: A => G[B])(
            implicit A: Applicative[G]): G[CopK[F ::: TNilK, B]] = {
          cfa match {
            case I(fa) => A.map(F.traverse(fa)(f))(I(_))
          }
        }
      }
    }
  }

  implicit def induct[F[_], LL <: TListK](
      implicit
      F: Traverse[F],
      LL: TraverseMaterializer[LL]
  ): TraverseMaterializer[F ::: LL] = new TraverseMaterializer[F ::: LL] {
    override def materialize(offset: Int): Traverse[CopK[F ::: LL, ?]] = {
      val I = mkInject[F, F ::: LL](offset)

      new DefaultTraverse[CopK[F ::: LL, ?]] {
        override def traverse[G[_], A, B](cfa: CopK[F ::: LL, A])(f: A => G[B])(
            implicit A: Applicative[G]): G[CopK[F ::: LL, B]] = {
          cfa match {
            case I(fa) => A.map(F.traverse(fa)(f))(I(_))
            case other =>
              LL.materialize(offset + 1)
                .traverse(other.asInstanceOf[CopK[LL, A]])(f)
                .asInstanceOf[G[iota.CopK[F ::: LL, B]]]
          }
        }
      }
    }
  }

}
