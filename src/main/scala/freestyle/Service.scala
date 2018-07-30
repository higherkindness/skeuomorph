/*
 * Copyright 2017-2018 47 Degrees, LLC. <http://www.47deg.com>
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

import turtles._
import turtles.implicits._

// We need to introduce an optimization here.  Transform request & response types of operations into
case class Service[T](pkg: String, name: String, declarations: List[T], operations: List[Service.Operation[T]])

object Service {

  import util._

  case class Operation[T](name: String, request: T, response: T)

  def namedTypes[T](t: T)(implicit T: Birecursive.Aux[T, Schema]): T = t.project match {
    case Schema.TProduct(name, _) => Schema.TNamedType[T](name).embed
    case Schema.TSum(name, _)     => Schema.TNamedType[T](name).embed
    case other                    => other.embed
  }

  def renderService[T](service: Service[T])(implicit T: Birecursive.Aux[T, Schema]): String = {
    val printDeclarations = service.declarations.map(_.cata(render)).mkString("\n")
    val printOperations = service.operations.map { op =>
      val printRequest  = op.request.transCataT(namedTypes).cata(render)
      val printResponse = op.response.transCataT(namedTypes).cata(render)

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
