/*
 * Copyright 2018-2023 47 Degrees Open Source <https://www.47deg.com>
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
import higherkindness.skeuomorph.mu.DependentImport

final case class Protocol[T](
    name: String,
    pkg: String,
    options: List[(String, String)],
    declarations: List[T],
    services: List[Protocol.Service[T]],
    imports: List[DependentImport[T]]
)

object Protocol {

  final case class Service[T](
      name: String,
      operations: List[Operation[T]]
  )

  final case class Operation[T](
      name: String,
      request: T,
      requestStreaming: Boolean,
      response: T,
      responseStreaming: Boolean
  )

}
