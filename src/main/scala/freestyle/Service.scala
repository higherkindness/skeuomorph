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
import Optimize.namedTypes
import Transform.transformAvro

import cats.instances.function._
import cats.syntax.compose._
import qq.droste._

case class Service[T](pkg: String, name: String, declarations: List[T], operations: List[Service.Operation[T]])

object Service {

  case class Operation[T](name: String, request: T, response: T)

  /**
   * create a [[skeuomorph.freestyle.Service]] from a [[skeuomorph.avro.Protocol]]
   */
  def fromAvroProtocol[T, U](proto: avro.Protocol[T])(implicit T: Basis[AvroF, T], U: Basis[FreesF, U]): Service[U] = {

    val toFreestyle: T => U = scheme.cata(transformAvro[U].algebra)
    val toOperation: avro.Protocol.Message[T] => Operation[U] =
      msg =>
        Service.Operation(
          msg.name,
          toFreestyle(msg.request),
          toFreestyle(msg.response)
      )

    Service(
      proto.namespace.fold("")(identity),
      proto.name,
      proto.types.map(toFreestyle),
      proto.messages.map(toOperation)
    )
  }

  def render[T](service: Service[T])(implicit T: Basis[FreesF, T]): String = {
    val renderFrees: T => String = scheme.cata(FreesF.render)
    val optimizeAndPrint         = namedTypes >>> renderFrees

    val printDeclarations = service.declarations.map(renderFrees).mkString("\n")
    val printOperations = service.operations.map { op =>
      val printRequest  = optimizeAndPrint(op.request)
      val printResponse = optimizeAndPrint(op.response)

      s"def ${op.name}(req: $printRequest): F[$printResponse]"
    } mkString ("\n  ")
    val printService = s"""
@service trait ${service.name}[F[_]] {
  $printOperations
}
"""
    s"""
package ${service.pkg}
$printDeclarations
$printService
"""
  }

}
