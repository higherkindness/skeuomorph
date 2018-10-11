/*
 * Copyright 2018 47 Degrees, LLC. <http://www.47deg.com>
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

package skeuomorph
package catz
package contrib

import cats.Contravariant

trait Divisible[F[_]] extends Contravariant[F] {
  def divide[A, B, C](fa: F[A], fb: F[B])(cab: C => (A, B)): F[C]
  def conquer[A]: F[A]
}

object Divisible {
  def apply[F[_]](implicit F: Divisible[F]): Divisible[F] = F

  implicit class divisibleSyntax[F[_]: Divisible, A](fa: F[A]) {
    def >*<[B](fb: F[B]): F[(A, B)] = Divisible[F].divide(fa, fb)(identity)
  }

  implicit class divisibleLeftForgetSyntax[F[_]: Divisible, A](fa: F[Unit]) {
    def *<[B](fb: F[B]): F[B] = Divisible[F].divide(fa, fb)(a => ((), a))
  }

  implicit class divisibleRightForgetSyntax[F[_]: Divisible, A](fa: F[A]) {
    def >*(fb: F[Unit]): F[A] = Divisible[F].divide(fa, fb)(a => (a, ()))
  }
}
