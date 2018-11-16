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
package freestyle

import avro.AvroF
import Transform.transformAvro

import qq.droste._

sealed trait SerializationType extends Product with Serializable
object SerializationType {
  case object Protobuf       extends SerializationType
  case object Avro           extends SerializationType
  case object AvroWithSchema extends SerializationType
}

final case class Protocol[T](
    name: String,
    pkg: Option[String],
    options: List[(String, String)],
    declarations: List[T],
    services: List[Service[T]]
)
object Protocol {

  /**
   * create a [[skeuomorph.freestyle.Service]] from a [[skeuomorph.avro.Protocol]]
   */
  def fromAvroProtocol[T, U](proto: avro.Protocol[T])(implicit T: Basis[AvroF, T], U: Basis[FreesF, U]): Protocol[U] = {

    val toFreestyle: T => U = scheme.cata(transformAvro[U].algebra)
    val toOperation: avro.Protocol.Message[T] => Service.Operation[U] =
      msg =>
        Service.Operation(
          msg.name,
          toFreestyle(msg.request),
          toFreestyle(msg.response)
      )

    Protocol(
      proto.name,
      proto.namespace,
      Nil,
      proto.types.map(toFreestyle),
      List(Service(proto.name, SerializationType.Avro, proto.messages.map(toOperation)))
    )
  }
}

final case class Service[T](name: String, serializationType: SerializationType, operations: List[Service.Operation[T]])
object Service {
  final case class Operation[T](name: String, request: T, response: T)
}
