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

import catz.contrib.Decidable
import cats.syntax.compose._
import cats.instances.function._

trait Printer[A] {
  def print(a: A): String
  def contramap[B](f: B => A): Printer[B] = Printer(f >>> print)
}

object Printer {

  def apply[A](f: A => String): Printer[A] = new Printer[A] {
    def print(a: A): String = f(a)
  }

  def konst(str: String): Printer[Unit] =
    Printer(Function.const(str))

  val space: Printer[Unit] = konst(" ")

  val newLine: Printer[Unit] = konst("\n")

  val string: Printer[String] = Printer(identity)

  val unit: Printer[Unit] = Printer(_ => "")

  def optional[A](p: Printer[A]): Printer[Option[A]] =
    Printer(_.fold("")(p.print))

  def sepBy[A](p: Printer[A], sep: String): Printer[List[A]] =
    Printer {
      case Nil => ""
      case xs  => xs.map(p.print).mkString(sep)
    }

  implicit val divisiblePrinter: Decidable[Printer] = new Decidable[Printer] {
    def unit: Printer[Unit] = Printer.unit
    def product[A, B](fa: Printer[A], fb: Printer[B]): Printer[(A, B)] = new Printer[(A, B)] {
      def print(ab: (A, B)): String = fa.print(ab._1) + fb.print(ab._2)
    }
    def contramap[A, B](fa: Printer[A])(f: B => A): Printer[B] = Printer(f >>> fa.print)
    def choose[A, B, C](fa: Printer[A], fb: Printer[B])(cab: C => Either[A, B]): Printer[C] =
      Printer(cab(_).fold(fa.print, fb.print))
  }

}
