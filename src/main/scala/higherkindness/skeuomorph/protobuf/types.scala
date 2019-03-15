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

package higherkindness.skeuomorph.protobuf

import higherkindness.skeuomorph.uast.types.FieldF
import higherkindness.skeuomorph.uast.derivation._
import qq.droste.macros.deriveTraverse
import cats.instances.list._

object types {

  // annotations used for declaring numeric types
  final case class `32`()
  final case class `64`()
  final case class Signed()
  final case class Unsigned()
  final case class Fixed()

  // todo: move somewhere else?
  @deriveTraverse final case class TProtoEnum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[OptionValue],
      aliases: List[(String, Int)])
  @deriveTraverse final case class TMessage[A](name: String, fields: List[FieldF[A]], reserved: List[List[String]])

}
