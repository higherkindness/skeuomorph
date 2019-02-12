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
import qq.droste._

final case class Protocol[T](
    name: String,
    pkg: String,
    options: List[(String, String)],
    imports: List[String],
    declarations: List[T],
    services: List[Protocol.Service[T]]
)

object Protocol {

  import ProtobufF._

  final case class Service[T](
      name: String,
      operations: List[Operation[T]]
  )

  final case class Operation[T](
      name: String,
      request: T,
      response: T
  )

  def fromProto[T](protocol: NativeFile)(implicit T: Embed[ProtobufF, T]): Protocol[T] = {
    val toProtobufF: NativeDescriptor => T = scheme.ana(fromProtobuf)

    def toService(s: NativeService): Service[T] =
      Service[T](s.name, s.operations.map(toOperation))

    def toOperation(o: NativeOperation): Operation[T] =
      Operation[T](
        name = o.name,
        request = toProtobufF(o.request),
        response = toProtobufF(o.request)
      )

    Protocol[T](
      name = protocol.name,
      pkg = protocol.`package`,
      options = Nil,
      imports = protocol.imports,
      declarations = protocol.values.map(m => toProtobufF(m)),
      services = protocol.services.map(s => toService(s))
    )
  }

}
