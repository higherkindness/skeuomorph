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
