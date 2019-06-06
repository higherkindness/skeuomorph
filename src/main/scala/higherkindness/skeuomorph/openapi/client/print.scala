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

package higherkindness.skeuomorph.openapi.client

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer._
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.openapi.JsonSchemaF
import cats.implicits._

import qq.droste._

object print {
  import higherkindness.skeuomorph.openapi.print.{componentsRegex, schema}
  import higherkindness.skeuomorph.openapi.schema._
  import higherkindness.skeuomorph.openapi.schema.Path._

  def parameter: Printer[(String, String)] =
    (
      string >* konst(": "),
      string
    ).contramapN(identity)

  def typeName[T: Basis[JsonSchemaF, ?]](t: T): Option[String] = Some(schema().print(t))

  def requestTuple[T: Basis[JsonSchemaF, ?]](request: Request[T]): Option[(String, String)] =
    for {
      content <- request.content.get("application/json")
      schema  <- content.schema
      name    <- typeName(schema)
    } yield (decapitalize(name), name.capitalize)

  def referenceTuple(reference: Reference): Option[(String, String)] = reference.ref match {
    case componentsRegex(name) => Some((decapitalize(name) -> name.capitalize))
    case _                     => None
  }

  def opTuple[T: Basis[JsonSchemaF, ?]](operation: OperationWithPath[T]): (String, (List[(String, String)]), String) =
    (
      operation._3.operationId.getOrElse(???), //FIXME What's happen when there is not operationId?
      operation._3.requestBody.flatMap(_.fold[Option[(String, String)]](requestTuple, referenceTuple)).toList,
      "Unit")

  type OperationWithPath[T] = (String, String, Operation[T])
  type TraitName            = String
  def method[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      konst("  def ") *< string,
      konst("(") *< sepBy(parameter, ","),
      konst("): F[") *< string >* konst("]")
    ).contramapN(opTuple[T])

  def itemObjectTuple[T: Basis[JsonSchemaF, ?]](xs: (String, ItemObject[T])): List[OperationWithPath[T]] = {
    val (path, itemObject) = xs
    List(
      itemObject.get.map(("get", path, _)),
      itemObject.delete.map(("delete", path, _)),
      itemObject.post.map(("post", path, _)),
      itemObject.put.map(("put", path, _))
    ).flatten
  }

  def reponseSchema[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] = Printer(_ => "")

  def clientTypes[T: Basis[JsonSchemaF, ?]]: Printer[(TraitName, List[OperationWithPath[T]])] =
    (konst("object ") *< string >* konst("Client {") >* newLine, sepBy(reponseSchema[T], "") >* (newLine >* konst("}")))
      .contramapN(identity)

  def operationsTuple[T: Basis[JsonSchemaF, ?]](x: (TraitName, Map[String, ItemObject[T]])): (
      TraitName,
      TraitName,
      List[OperationWithPath[T]],
      (TraitName, List[OperationWithPath[T]])) =
    un(second(duplicate(second(x)(_.toList.flatMap(itemObjectTuple[T])))) { case (a, b) => (a, (x._1, b)) })

  def operations[T: Basis[JsonSchemaF, ?]]: Printer[(TraitName, Map[String, ItemObject[T]])] =
    (
      konst("trait ") *< string >* konst("Client[F[_]] {") >* newLine,
      (space >* space >* konst("import ")) *< string >* konst("Client._") >* newLine,
      sepBy(method[T], "\n") >* (newLine >* konst("}") >* newLine),
      clientTypes[T]
    ).contramapN(operationsTuple[T])

  def un[A, B, C, D](pair: ((A, B), (C, D))): (A, B, C, D) = (pair._1._1, pair._1._2, pair._2._1, pair._2._2)
  def duplicate[A, B](pair: (A, B)): ((A, A), (B, B))      = (pair._1 -> pair._1, pair._2 -> pair._2)
  def second[A, B, C](pair: (A, B))(f: B => C): (A, C)     = (pair._1, f(pair._2))
  def decapitalize(s: String)                              = if (s.isEmpty) s else s(0).toLower + s.substring(1)

}
