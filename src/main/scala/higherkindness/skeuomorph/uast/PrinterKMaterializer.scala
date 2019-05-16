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

package higherkindness
package skeuomorph
package uast

import iota.TListK.:::
import iota.{CopK, TListK, TNilK}

sealed trait PrinterKMaterializer[LL <: TListK] {
  def materialize(offset: Int): Delay[Printer, CopK[LL, ?]]
}

object PrinterKMaterializer {

  implicit def base[F[_]](
      implicit
      F: Delay[Printer, F]
  ): PrinterKMaterializer[F ::: TNilK] = new PrinterKMaterializer[F ::: TNilK] {
    override def materialize(offset: Int): Delay[Printer, CopK[F ::: TNilK, ?]] = {
      val I = mkInject[F, F ::: TNilK](offset)
      new Delay[Printer, λ[α => CopK[F ::: TNilK, α]]] {
        override def apply[A](printer: Printer[A]): Printer[CopK[F ::: TNilK, A]] =
          Printer {
            case I(x) => F(printer).print(x)
          }
      }
    }
  }

  implicit def induct[F[_], LL <: TListK](
      implicit
      F: Delay[Printer, F],
      LL: PrinterKMaterializer[LL]
  ): PrinterKMaterializer[F ::: LL] = new PrinterKMaterializer[F ::: LL] {
    override def materialize(offset: Int): Delay[Printer, CopK[F ::: LL, ?]] = {
      val I = mkInject[F, F ::: LL](offset)
      new Delay[Printer, λ[α => CopK[F ::: LL, α]]] {
        override def apply[A](printer: Printer[A]): Printer[CopK[F ::: LL, A]] =
          Printer {
            case (I(x)) => F(printer).print(x)
            case other  => LL.materialize(offset + 1)(printer).print(other.asInstanceOf[CopK[LL, A]])
          }
      }
    }
  }

}
