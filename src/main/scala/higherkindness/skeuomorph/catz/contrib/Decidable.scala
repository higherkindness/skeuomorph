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

trait Decidable[F[_]] extends ContravariantMonoidal[F] {
  def choose[A, B, C](fa: F[A], fb: F[B])(cab: C => Either[A, B]): F[C]
}

object Decidable {
  def apply[F[_]](implicit F: Decidable[F]): Decidable[F] = F

  implicit class decidableSyntax[F[_]: Decidable, A](fa: F[A]) {
    def >|<[B](fb: F[B]): F[Either[A, B]] = Decidable[F].choose(fa, fb)(identity)
  }
}
