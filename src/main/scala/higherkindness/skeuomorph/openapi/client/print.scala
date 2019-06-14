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
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi._
import cats.implicits._
import cats.Show

import qq.droste._

object print {
  import higherkindness.skeuomorph.openapi.print.{componentsRegex, schema, schemaWithName}
  import higherkindness.skeuomorph.openapi.schema._
  import higherkindness.skeuomorph.openapi.schema.Path._

  private val jsonMediaType = "application/json"

  def normalize(value: String): String = value.split(" ").map(_.filter(_.isLetter).capitalize).mkString

  case class Tpe[T](tpe: Either[String, T], required: Boolean, description: String)
  object Tpe {
    def unit[T]: Tpe[T]                = Tpe("Unit".asLeft, true, "Unit")
    def apply[T](name: String): Tpe[T] = Tpe(name.asLeft, true, name)
    def apply[T](tpe: T, required: Boolean, description: String): Tpe[T] =
      Tpe(tpe.asRight, required, description)

    def name[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): String = tpe.tpe.fold(
      identity,
      x =>
        Printer(Optimize.namedTypes[T](normalize(tpe.description)) >>> schema(none).print)
          .print(x)
          .capitalize
    )
    def option[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): Either[String, String] =
      if (tpe.required)
        name(tpe).asRight
      else
        name(tpe).asLeft

  }
  case class Var[T](name: String, tpe: Tpe[T])
  object Var {
    def tpe[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): Var[T] = Var(decapitalize(Tpe.name(tpe)), tpe)
    def apply[T](name: String, tpe: T, required: Boolean, description: String): Var[T] =
      Var(decapitalize(name), Tpe(tpe.asRight, required, description))
  }

  type HttpVerb                    = String
  type HttpPath                    = String
  type OperationWithPath[T]        = (HttpVerb, HttpPath, Operation[T])
  type ResponsesWithOperationId[T] = (OperationId, Map[String, Either[Response[T], Reference]])
  final case class TraitName(value: String) extends AnyVal
  object TraitName {
    def apply[T](openApi: OpenApi[T]): TraitName = TraitName(s"${normalize(openApi.info.title)}Client")
    implicit val traitNameShow: Show[TraitName]  = Show.show(_.value)
  }
  final case class ImplName(value: String) extends AnyVal
  object ImplName {
    def apply[T](openApi: OpenApi[T]): ImplName = ImplName(s"${normalize(openApi.info.title)}HttpClient")
    implicit val implNameShow: Show[ImplName]   = Show.show(_.value)
  }

  final case class PackageName(value: String) extends AnyVal
  object PackageName {
    implicit val packageNameShow: Show[PackageName] = Show.show(_.value)
  }

  final case class OperationId(value: String) extends AnyVal

  object OperationId {

    def from(name: String): OperationId = OperationId(normalize(name))

    def apply[T](operationWithPath: OperationWithPath[T]): OperationId =
      OperationId(operationWithPath._3.operationId.getOrElse {
        val verb = operationWithPath._1.toLowerCase() match {
          case "post" => "create"
          case "put"  => "update"
          case x      => x
        }
        val path =
          operationWithPath._2
            .split("/")
            .filterNot(_.contains("{"))
            .filter(_.nonEmpty)
            .reverse
            .map(_.capitalize)
            .mkString
        s"${verb}${path}"
      })

    implicit val operationIdShow: Show[OperationId] = Show.show(_.value)
  }

  private def tpe[T: Basis[JsonSchemaF, ?]]: Printer[Tpe[T]] =
    ((konst("Option[") *< string >* konst("]")) >|< string)
      .contramap(Tpe.option[T])

  def imports: Printer[List[PackageName]] =
    (
      sepBy(
        (konst("import ") *< show[PackageName]),
        "\n")
    ).contramap(identity)

  private def parameter[T: Basis[JsonSchemaF, ?]]: Printer[Var[T]] =
    (
      string >* konst(": "),
      tpe
    ).contramapN(x => x.name -> x.tpe)

  private def requestTuple[T: Basis[JsonSchemaF, ?]](operationId: OperationId, request: Request[T]): Option[Var[T]] =
    request.content
      .get(jsonMediaType)
      .flatMap(x =>
        x.schema.map(x =>
          Var.tpe[T](Tpe(x, request.required, request.description.getOrElse(defaultRequestName(operationId))))))

  private def referenceTuple[T: Basis[JsonSchemaF, ?]](reference: Reference): Option[Var[T]] = reference.ref match {
    case componentsRegex(name) => Var.tpe(Tpe.apply(name)).some
    case _                     => none
  }

  def requestOrTuple[T: Basis[JsonSchemaF, ?]](
      operationId: OperationId,
      requestOr: Either[Request[T], Reference]): Option[Var[T]] =
    requestOr.fold[Option[Var[T]]](requestTuple(operationId, _), referenceTuple)

  private def parameterTuple[T: Basis[JsonSchemaF, ?]](parameter: Either[Parameter[T], Reference]): Option[Var[T]] =
    parameter.fold(
      x => Var(x.name, x.schema, x.required, x.description.getOrElse(x.name)).some,
      referenceTuple[T]
    )

  private def operationTuple[T: Basis[JsonSchemaF, ?]](
      operation: OperationWithPath[T]): (OperationId, (List[Var[T]]), ResponsesWithOperationId[T]) = {
    val operationId = OperationId(operation)
    (
      operationId,
      operation._3.parameters.flatMap(parameterTuple[T]) ++
        operation._3.requestBody
          .flatMap(requestOrTuple[T](operationId, _)),
      operationId -> operation._3.responses)
  }

  private def defaultRequestName[T](operationId: OperationId): String =
    s"${operationId.show}Request".capitalize

  private def defaultResponseName[T](operationId: OperationId): String =
    s"${operationId.show}Response".capitalize

  private def typeFromResponse[T: Basis[JsonSchemaF, ?]](response: Response[T]): Option[T] =
    response.content.get(jsonMediaType).flatMap(_.schema)

  private def responseType[T: Basis[JsonSchemaF, ?]]: Printer[Response[T]] =
    tpe.contramap(
      response =>
        typeFromResponse(response)
          .map(Tpe(_, true, normalize(response.description)))
          .getOrElse(Tpe.unit))

  private def referenceTpe[T: Basis[JsonSchemaF, ?]](x: Reference): Tpe[T] =
    referenceTuple(x).map(_.tpe).getOrElse(Tpe.unit)

  private def responseOrType[T: Basis[JsonSchemaF, ?]]: Printer[Either[Response[T], Reference]] =
    responseType >|< tpe.contramap(referenceTpe[T])

  private def responsesTypes[T: Basis[JsonSchemaF, ?]]: Printer[ResponsesWithOperationId[T]] = Printer {
    case (x, y) =>
      val defaultName = defaultResponseName(x)
      y.size match {
        case 0 => "Unit"
        case 1 => responseOrType.print(y.head._2)
        case _ => defaultName
      }
  }

  def method[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      konst("  def ") *< show[OperationId],
      konst("(") *< sepBy(parameter, ", "),
      konst("): F[") *< responsesTypes >* konst("]")
    ).contramapN(operationTuple[T])

  private def itemObjectTuple[T](xs: (String, ItemObject[T])): List[OperationWithPath[T]] = {
    val (path, itemObject) = xs
    List(
      itemObject.get.map(("get", path, _)),
      itemObject.delete.map(("delete", path, _)),
      itemObject.post.map(("post", path, _)),
      itemObject.put.map(("put", path, _))
    ).flatten
  }

  private def typeAndSchemaFor[T: Basis[JsonSchemaF, ?], X](response: Response[T], tpe: String)(
      success: => (String, List[String])): (String, List[String]) = {
    val newSchema =
      typeFromResponse(response).map(schema(normalize(response.description).some).print).getOrElse("")
    tpe match {
      case _ if (tpe === newSchema)  => success
      case _ if (newSchema.nonEmpty) => normalize(response.description) -> List(newSchema)
      case _                         => tpe -> Nil
    }
  }

  private def newCaseClass(name: String, fields: (String, String)*): String =
    s"final case class $name(${fields.map { case (x, y) => s"$x: $y" }.mkString(", ")})"

  private def typesAndSchemas[T: Basis[JsonSchemaF, ?]](
      status: String,
      responseOr: Either[Response[T], Reference]): (String, List[String]) = {
    val tpe        = responseOrType.print(responseOr)
    val intPattern = """(\d+)""".r
    (status match {
      case intPattern(code) =>
        responseOr.left.toOption.map(
          response =>
            typeAndSchemaFor(response, tpe) {
              if (code.toInt >= 200 && code.toInt < 400)
                tpe -> Nil
              else {
                val newTpe = defaultResponseName(OperationId.from(response.description))
                newTpe -> List(newCaseClass(newTpe, "value" -> tpe))
              }
          }
        )
      case "default" =>
        val newType = "UnexpectedErrorResponse"
        val x: List[String] = responseOr.left.toOption.toList.flatMap(
          typeAndSchemaFor(_, tpe) { tpe -> List.empty }._2
        ) ++ List(newCaseClass(newType, "statusCode" -> "Int", "value" -> tpe))
        (newType -> x).some
    }).getOrElse(tpe -> List.empty)
  }

  private def responsesSchemaTuple[T: Basis[JsonSchemaF, ?]](
      defaultName: String,
      responses: Map[String, Either[Response[T], Reference]]): Either[
    (List[String], String, List[String]),
    List[String]] =
    if (responses.size > 1) {
      val (newTypes, schemas) = second(responses.map((typesAndSchemas[T] _).tupled).toList.unzip)(_.flatten)
      (schemas.toList.filter(_.nonEmpty), defaultName, newTypes.toList).asLeft
    } else
      responses.toList
        .map(_._2)
        .flatMap(r => r.left.toOption.map(_ -> responseOrType.print(r)))
        .flatMap { case (response, tpe) => typeAndSchemaFor(response, tpe) { tpe -> Nil }._2 }
        .asRight

  private def multipleResponsesSchema: Printer[(List[String], String, List[String])] =
    (
      sepBy((space >* space) *< string, "\n") >* newLine,
      konst("  type ") *< string >* konst(" = "),
      sepBy(string, " :+: ") >* konst(" :+: CNil"))
      .contramapN(identity)

  private def responsesSchema[T: Basis[JsonSchemaF, ?]]: Printer[
    (String, Map[String, Either[Response[T], Reference]])] =
    (multipleResponsesSchema >|< sepBy((space >* space) *< string, "\n")).contramap((responsesSchemaTuple[T] _).tupled)

  private def requestSchemaTuple[T: Basis[JsonSchemaF, ?]](
      operationId: OperationId,
      request: Option[Either[Request[T], Reference]]): Option[(String, T)] =
    for {
      request <- request.flatMap(_.left.toOption)
      requestTuple <- requestTuple(operationId, request)
        .map(_.tpe)
      requestTpe <- requestTuple.tpe.toOption
      name    = tpe.print(requestTuple)
      newType = defaultRequestName(operationId)
      if name === newType
    } yield (newType -> requestTpe)

  private def requestSchema[T: Basis[JsonSchemaF, ?]]: Printer[(OperationId, Option[Either[Request[T], Reference]])] =
    (optional((space >* space) *< schemaWithName[T])).contramap((requestSchemaTuple[T] _).tupled)

  private def clientTypes[T: Basis[JsonSchemaF, ?]]: Printer[(TraitName, List[OperationWithPath[T]])] =
    (
      konst("object ") *< show[TraitName] >* konst(" {") >* newLine,
      sepBy(requestSchema[T], "\n") >* newLine,
      sepBy(responsesSchema[T], "\n") >* (newLine >* konst("}")))
      .contramapN { x =>
        un(second(x)(_.map { x =>
          val operationId = OperationId(x)
          (operationId                        -> x._3.requestBody) ->
            (defaultResponseName(operationId) -> x._3.responses)
        }.unzip))
      }

  def toOperationsWithPath[T](x: (TraitName, Map[String, ItemObject[T]])): (TraitName, List[OperationWithPath[T]]) =
    second(x)(_.toList.flatMap(itemObjectTuple[T]))

  private def operationsTuple[T: Basis[JsonSchemaF, ?]](x: (TraitName, Map[String, ItemObject[T]])): (
      List[PackageName],
      TraitName,
      TraitName,
      List[OperationWithPath[T]],
      (TraitName, List[OperationWithPath[T]])) = {
    val (a, b, c, d) = un(second(duplicate(toOperationsWithPath(x))) { case (a, b) => (a, (x._1, b)) })
    (List("models._", "shapeless.{:+:, CNil}").map(PackageName.apply), a, b, c, d)
  }

  def operations[T: Basis[JsonSchemaF, ?]]: Printer[(TraitName, Map[String, ItemObject[T]])] =
    (
      imports >* newLine,
      konst("trait ") *< show[TraitName] >* konst("[F[_]] {") >* newLine,
      space *< space *< konst("import ") *< show[TraitName] >* konst("._") >* newLine,
      sepBy(method[T], "\n") >* (newLine >* konst("}") >* newLine),
      clientTypes[T]
    ).contramapN(operationsTuple[T])

  def un[A, B, C, D](pair: ((A, B), (C, D))): (A, B, C, D) = (pair._1._1, pair._1._2, pair._2._1, pair._2._2)
  def un[A, C, D](pair: (A, (C, D))): (A, C, D)            = (pair._1, pair._2._1, pair._2._2)
  def duplicate[A, B](pair: (A, B)): ((A, A), (B, B))      = (pair._1 -> pair._1, pair._2 -> pair._2)
  def second[A, B, C](pair: (A, B))(f: B => C): (A, C)     = (pair._1, f(pair._2))
  def decapitalize(s: String)                              = if (s.isEmpty) s else s(0).toLower + s.substring(1)

}
