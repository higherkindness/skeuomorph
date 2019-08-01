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
package avro

import cats.{Eq, Foldable}
import cats.instances.list._
import cats.data.NonEmptyList
import droste.macros.deriveFixedPoint
import cats.Monoid

object types {

  sealed trait Order
  object Order {
    case object Ascending  extends Order
    case object Descending extends Order
    case object Ignore     extends Order

    implicit val orderEq: Eq[Order] = Eq.instance {
      case (Ascending, Ascending)   => true
      case (Descending, Descending) => true
      case (Ignore, Ignore)         => true
      case _                        => false
    }
  }

  @deriveFixedPoint sealed trait AvroMetadata
  object AvroMetadata {
    case class Aliases(aliases: NonEmptyList[String]) extends AvroMetadata
    case class NameSpace(namespace: String)           extends AvroMetadata
    case class Doc(doc: String)                       extends AvroMetadata
    case class AMOrder(order: Order)                  extends AvroMetadata
    case class AMList(list: List[AvroMetadata])       extends AvroMetadata

    def fromParts(
      namespace: Option[String],
      aliases: List[String],
      doc: Option[String],
      order: Option[Order]
    ): AvroMetadata =
      Foldable[List].fold(
        namespace.map(NameSpace).toList ++
          NonEmptyList.fromList(aliases).map(Aliases).toList ++
          doc.map(Doc).toList ++
          order.map(AMOrder).toList)



    implicit val eq: Eq[AvroMetadata] = Eq.fromUniversalEquals
    implicit val monoid: Monoid[AvroMetadata] = new Monoid[AvroMetadata] {
      def empty = AMList(Nil)
      def combine(x: AvroMetadata, y: AvroMetadata): AvroMetadata = (x, y) match {
        case (AMList(x), AMList(y)) => AMList(x ++ y)
        case (AMList(x), y)         => AMList(y :: x)
        case (x, AMList(y))         => AMList(x :: y)
        case (x, y)                 => AMList(List(x, y))
      }
    }
  }
}
