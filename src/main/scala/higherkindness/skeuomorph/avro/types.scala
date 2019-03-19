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

package higherkindness.skeuomorph
package avro

import cats.Eq

object types {

  sealed trait Order
  object Order {
    case object Ascending  extends Order
    case object Descending extends Order
    case object Ignore     extends Order

    implicit def orderEq: Eq[Order] = Eq.instance {
      case (Ascending, Ascending)   => true
      case (Descending, Descending) => true
      case (Ignore, Ignore)         => true
      case _                        => false
    }
  }

  sealed trait AvroMetadata
  object AvroMetadata {
    case class Aliases(aliases: List[String])       extends AvroMetadata
    case class NameSpace(namespace: Option[String]) extends AvroMetadata
    case class Doc(doc: Option[String])             extends AvroMetadata
    case class AMOrder(order: Option[Order])        extends AvroMetadata
    case class AMList(list: List[AvroMetadata])     extends AvroMetadata

    implicit val eq: Eq[AvroMetadata] = Eq.fromUniversalEquals
  }
}
