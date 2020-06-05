/*
 * Copyright 2018-2020 47 Degrees <https://www.47deg.com>
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

package higherkindness.skeuomorph.mu.comparison

import cats.Show

sealed trait PathElement extends Product with Serializable
object PathElement {
  final case class Name(value: String)        extends PathElement
  final case class FieldName(name: String)    extends PathElement
  final case object Value                     extends PathElement
  final case object Values                    extends PathElement
  final case object Keys                      extends PathElement
  final case object Items                     extends PathElement
  final case class Alternative(idx: Int)      extends PathElement
  final case object LeftBranch                extends PathElement
  final case object RightBranch               extends PathElement
  final case object GenericType               extends PathElement
  final case class GenericParameter(idx: Int) extends PathElement

  implicit val pathElementShow: Show[PathElement] = Show.show {
    case Name(v)             => v
    case FieldName(n)        => n
    case Value               => "$value"
    case Values              => "$values"
    case Keys                => "$keys"
    case Items               => "$items"
    case Alternative(i)      => s"$$alt[$i]"
    case LeftBranch          => "$left"
    case RightBranch         => "$right"
    case GenericType         => "$gtype"
    case GenericParameter(i) => s"$$tparam[$i]"
  }
}

final case class Path(elements: Vector[PathElement]) {
  def /(elem: PathElement) = Path(elements :+ elem)
}
object Path {
  def empty: Path = Path(Vector.empty)

  implicit def pathShow(implicit elem: Show[PathElement]): Show[Path] =
    Show.show(_.elements.map(elem.show).mkString("."))

}
