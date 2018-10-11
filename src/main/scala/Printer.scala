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

case class Printer[A](print: A => String)

object Printer {

  def konst(str: String): Printer[Unit] =
    Printer(Function.const(str))

  def space = konst(" ")

  def newLine = konst("\n")

  val string: Printer[String] = Printer(identity)

  def optional[A](p: Printer[A]): Printer[Option[A]] =
    Printer(_.fold("")(p.print))

  def mkList[A](p: Printer[A], sep: String): Printer[List[A]] =
    Printer(_ match {
      case Nil => ""
      case xs  => xs.map(p.print).mkString(sep)
    })

  implicit val divisiblePrinter: Decidable[Printer] = new Decidable[Printer] {
    def contramap[A, B](fa: Printer[A])(f: B => A): Printer[B] =
      Printer(f >>> fa.print)
    def conquer[A]: Printer[A] =
      Printer(Function.const(""))
    def divide[A, B, C](fa: Printer[A], fb: Printer[B])(cab: C => (A, B)): Printer[C] =
      Printer(c =>
        cab(c) match {
          case (a, b) => fa.print(a) + fb.print(b)
      })
    def choose[A, B, C](fa: Printer[A], fb: Printer[B])(cab: C => Either[A, B]): Printer[C] =
      Printer(cab(_).fold(fa.print, fb.print))
  }

}
