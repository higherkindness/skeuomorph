/*
 * Copyright 2018-2023 47 Degrees Open Source <https://www.47deg.com>
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

package higherkindness.skeuomorph.catz.contrib

import cats.ContravariantMonoidal

object ContravariantMonoidalSyntax {
  def apply[F[_]](implicit F: ContravariantMonoidal[F]): ContravariantMonoidal[F] = F

  implicit class divisibleSyntax[F[_]: ContravariantMonoidal, A](fa: F[A]) {
    def >*<[B](fb: F[B]): F[(A, B)] = ContravariantMonoidal.contramap2(fa, fb)(identity)
  }

  implicit class divisibleLeftForgetSyntax[F[_]: ContravariantMonoidal, A](fa: F[Unit]) {
    def *<[B](fb: F[B]): F[B] = ContravariantMonoidal.contramap2(fa, fb)(a => ((), a))
  }

  implicit class divisibleRightForgetSyntax[F[_]: ContravariantMonoidal, A](fa: F[A]) {
    def >*(fb: F[Unit]): F[A] = ContravariantMonoidal.contramap2(fa, fb)(a => (a, ()))
  }
}
