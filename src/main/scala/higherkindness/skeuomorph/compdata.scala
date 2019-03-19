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

import cats.{~>, Applicative, Eq, Traverse}

import qq.droste.Delay
import qq.droste.util.DefaultTraverse
import qq.droste.syntax.compose._

object compdata {

  // constant product on signatures
  //
  // similar to compdata's :*:
  final case class Ann[F[_], E, A](fa: F[A], ann: E)
  object Ann {
    implicit def traverse[F[_], E](implicit F: Traverse[F]): Traverse[Ann[F, E, ?]] =
      new DefaultTraverse[Ann[F, E, ?]] {
        def traverse[G[_], A, B](fa: Ann[F, E, A])(f: A => G[B])(implicit G: Applicative[G]): G[Ann[F, E, B]] =
          fa match {
            case Ann(fa, ann) => G.map(F.traverse(fa)(f))(fb => Ann(fb, ann))
          }
      }
    def delayEq[F[_], E](implicit F: Delay[Eq, F], E: Eq[E]): Delay[Eq, Ann[F, E, ?]] =
      new (Eq ~> (Eq ∘ Ann[F, E, ?])#λ) {
        def apply[A](A: Eq[A]): Eq[Ann[F, E, A]] = new Eq[Ann[F, E, A]] {
          def eqv(a: Ann[F, E, A], b: Ann[F, E, A]): Boolean = (a, b) match {
            case (Ann(fa1, e1), Ann(fa2, e2)) =>
              Eq[E].eqv(e1, e2) && F(A).eqv(fa1, fa2)
          }
        }
      }
  }
}
