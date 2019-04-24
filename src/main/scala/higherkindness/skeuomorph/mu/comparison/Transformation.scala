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

package higherkindness.skeuomorph.mu.comparison

import cats.Show
import cats.syntax.show._

sealed trait Transformation[T]

object Transformation {
  final case class NumericWiddening[T](relativePath: Path, from: T, to: T)  extends Transformation[T]
  final case class StringConversion[T](relativePath: Path, from: T, to: T)  extends Transformation[T]
  final case class Addition[T](relativePath: Path, added: T)                extends Transformation[T]
  final case class Removal[T](relativePath: Path, removed: T)               extends Transformation[T]
  final case class MadeOptional[T](relativePath: Path)                      extends Transformation[T]
  final case class PromotedToEither[T](relativePath: Path, either: T)       extends Transformation[T]
  final case class PromotedToCoproduct[T](relativePath: Path, coproduct: T) extends Transformation[T]
  final case class CoproductWiddening[T](relativePath: Path, coproduct: T)  extends Transformation[T]

  implicit def transformationShow[T](implicit showT: Show[T]): Show[Transformation[T]] = Show.show {
    case NumericWiddening(p, f, t) => p.show ++ ": numeric widdening from " ++ f.show ++ " to " ++ t.show
    case StringConversion(p, f, t) => p.show ++ ": string conversion from " ++ f.show ++ " to " ++ t.show
    case Addition(p, a)            => p.show ++ ": added field with schema " ++ a.show
    case Removal(p, _)             => p.show ++ ": field removed"
    case MadeOptional(p)           => p.show ++ ": made optional"
    case PromotedToEither(p, e)    => p.show ++ ": promoted to either " ++ e.show
    case PromotedToCoproduct(p, c) => p.show ++ ": promoted to coproduct " ++ c.show
    case CoproductWiddening(p, c)  => p.show ++ ": coproduct widdening to " ++ c.show
  }
}

sealed trait Incompatibility

object Incompatibility {
  final case class Different(relativePath: Path)          extends Incompatibility
  final case class UnionMemberRemoved(relativePath: Path) extends Incompatibility

  implicit val incompatibilityShow: Show[Incompatibility] = Show.show {
    case Different(p)          => p.show ++ ": !!DIFFERENT!!"
    case UnionMemberRemoved(p) => p.show ++ ": union member not found in reader schema"
  }
}
