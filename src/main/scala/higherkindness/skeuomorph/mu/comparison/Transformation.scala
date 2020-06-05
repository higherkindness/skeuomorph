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
import cats.syntax.show._

final case class At[T](path: Path, transformation: Transformation[T])

sealed trait Transformation[T]
object Transformation {
  final case class NumericWidening[T](from: T, to: T)    extends Transformation[T]
  final case class StringConversion[T](from: T, to: T)   extends Transformation[T]
  final case class Addition[T](added: T)                 extends Transformation[T]
  final case class Removal[T](removed: T)                extends Transformation[T]
  final case class PromotionToOption[T]()                extends Transformation[T]
  final case class PromotionToEither[T](either: T)       extends Transformation[T]
  final case class PromotionToCoproduct[T](coproduct: T) extends Transformation[T]
  final case class CoproductWidening[T](coproduct: T)    extends Transformation[T]

  implicit def transformationShow[T](implicit showT: Show[T]): Show[Transformation[T]] =
    Show.show {
      case NumericWidening(f, t)   => "numeric widening from " ++ f.show ++ " to " ++ t.show
      case StringConversion(f, t)  => "string conversion from " ++ f.show ++ " to " ++ t.show
      case Addition(a)             => "added field with schema " ++ a.show
      case Removal(_)              => "field removed"
      case PromotionToOption()     => "promotion to option"
      case PromotionToEither(e)    => "promotion to either " ++ e.show
      case PromotionToCoproduct(c) => "promotion to coproduct " ++ c.show
      case CoproductWidening(c)    => "coproduct widening to " ++ c.show
    }
}

sealed trait Incompatibility

object Incompatibility {
  final case class Different(path: Path)          extends Incompatibility
  final case class UnionMemberRemoved(path: Path) extends Incompatibility

  implicit val incompatibilityShow: Show[Incompatibility] = Show.show {
    case Different(p)          => p.show ++ ": !!DIFFERENT!!"
    case UnionMemberRemoved(p) => p.show ++ ": union member not found in reader schema"
  }
}
