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
package avro

import org.apache.avro.{Schema => AvroSchema, Protocol => AvroProtocol}
import scala.collection.JavaConverters._

import qq.droste._

case class Protocol[A](
    name: String,
    namespace: Option[String],
    types: List[A],
    messages: List[Protocol.Message[A]]
)

object Protocol {
  import AvroF._

  case class Message[A](name: String, request: A, response: A)

  def fromProto[T](proto: AvroProtocol)(implicit T: Embed[Schema, T]): Protocol[T] = {
    val toAvroSchema: AvroSchema => T = scheme.ana(fromAvro)
    def toMessage(kv: (String, AvroProtocol#Message)): Message[T] =
      Message[T](kv._2.getName, toAvroSchema(kv._2.getRequest), toAvroSchema(kv._2.getResponse))

    Protocol(
      proto.getName,
      Option(proto.getNamespace),
      proto.getTypes.asScala.toList.map(toAvroSchema),
      proto.getMessages.asScala.toList.map(toMessage)
    )
  }

}
