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
import higherkindness.skeuomorph.Printer.{konst => κ, _}
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi._
import higherkindness.skeuomorph.openapi.print._
import cats.implicits._
import cats.Show

import qq.droste._

object print {
  import higherkindness.skeuomorph.openapi.print.{componentsRegex, parametersRegex, schema, schemaWithName}
  import higherkindness.skeuomorph.openapi.schema._
  import higherkindness.skeuomorph.openapi.schema.Path._

  private val jsonMediaType = "application/json"
  private val allContent    = "*/*"

  def jsonFrom[T](content: Map[String, MediaType[T]]): Option[MediaType[T]] =
    content.get(jsonMediaType) orElse content.get(allContent)

  val statusCodePattern = """(\d+)""".r
  def successStatusCode(code: String): Boolean = code match {
    case statusCodePattern(code) => code.toInt >= 200 && code.toInt < 400
    case _                       => false
  }

  def componentsFrom[T](openApi: OpenApi[T]): Components[T] =
    openApi.components.getOrElse(Components(Map.empty, Map.empty, Map.empty, Map.empty))

  sealed trait HttpVerb

  object HttpVerb {
    final case object Put    extends HttpVerb
    final case object Post   extends HttpVerb
    final case object Get    extends HttpVerb
    final case object Delete extends HttpVerb

    def methodFrom: HttpVerb => String = {
      case Post => "create"
      case Put  => "update"
      case x    => x.show.toLowerCase()
    }

    implicit val httpVerbShow: Show[HttpVerb] = Show.show {
      case Put    => "PUT"
      case Post   => "POST"
      case Get    => "GET"
      case Delete => "DELETE"
    }
  }

  final case class HttpPath(value: String) extends AnyVal {
    def method: String =
      value
        .split("/")
        .filterNot(_.contains("{"))
        .filter(_.nonEmpty)
        .reverse
        .map(_.capitalize)
        .mkString
  }
  object HttpPath {
    implicit val httpPathShow: Show[HttpPath] = Show.show(_.value)
  }

  case class OperationWithPath[T](
      verb: HttpVerb,
      path: HttpPath,
      description: Option[String],
      operationId: OperationId,
      parameters: List[Parameter[T]],
      requestBody: Option[Either[Request[T], Reference]],
      responses: Map[String, Either[Response[T], Reference]]
  )
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

  final case class OperationId private (value: String) extends AnyVal

  object OperationId {

    def apply[T](operationWithPath: (HttpVerb, HttpPath, Path.Operation[T])): OperationId =
      OperationId(decapitalize(normalize(operationWithPath._3.operationId.getOrElse {
        s"${HttpVerb.methodFrom(operationWithPath._1)}${operationWithPath._2.method}"
      })))

    implicit val operationIdShow: Show[OperationId] = Show.show(_.value)
  }

  private def requestTuple[T: Basis[JsonSchemaF, ?]](
      operationId: OperationId,
      request: Request[T]): Option[VarWithType[T]] =
    jsonFrom(request.content)
      .flatMap(x =>
        x.schema.map(x =>
          VarWithType.tpe[T](Tpe(x, request.required, request.description.getOrElse(defaultRequestName(operationId))))))

  private def referenceTuple[T: Basis[JsonSchemaF, ?]](reference: Reference): Option[VarWithType[T]] =
    reference.ref match {
      case componentsRegex(name) => VarWithType.tpe(Tpe.apply(name)).some
      case _                     => none
    }

  def requestOrTuple[T: Basis[JsonSchemaF, ?]](
      operationId: OperationId,
      requestOr: Either[Request[T], Reference]): Option[VarWithType[T]] =
    requestOr.fold[Option[VarWithType[T]]](requestTuple(operationId, _), referenceTuple)

  private def parameterTuple[T: Basis[JsonSchemaF, ?]](x: Parameter[T]): VarWithType[T] =
    VarWithType(x.name, x.schema, x.required, x.description.getOrElse(x.name))

  private def operationTuple[T: Basis[JsonSchemaF, ?]](
      operation: OperationWithPath[T]): (OperationId, (List[VarWithType[T]]), ResponsesWithOperationId[T]) =
    (
      operation.operationId,
      operation.parameters.map(parameterTuple[T]) ++
        operation.requestBody
          .flatMap(requestOrTuple[T](operation.operationId, _)),
      operation.operationId -> operation.responses)

  private def defaultRequestName(operationId: OperationId): String =
    s"${operationId.show}Request".capitalize

  case class TypeAliasErrorResponse(operationId: OperationId)

  object TypeAliasErrorResponse {
    implicit val typeAliasErrorResponse: Show[TypeAliasErrorResponse] =
      Show.show(x => s"${x.operationId.show}ErrorResponse".capitalize)
  }

  private def typeFromResponse[T: Basis[JsonSchemaF, ?]](response: Response[T]): Option[T] =
    jsonFrom(response.content).flatMap(_.schema)

  private def responseType[T: Basis[JsonSchemaF, ?]]: Printer[Response[T]] =
    tpe.contramap { response =>
      typeFromResponse(response)
        .map(Tpe(_, true, normalize(response.description)))
        .getOrElse(Tpe.unit)
    }

  private def referenceTpe[T: Basis[JsonSchemaF, ?]](x: Reference): Tpe[T] =
    referenceTuple(x).map(_.tpe).getOrElse(Tpe.unit)

  def responseOrType[T: Basis[JsonSchemaF, ?]]: Printer[Either[Response[T], Reference]] =
    responseType >|< tpe.contramap(referenceTpe[T])

  def successType[T](responses: Map[String, Either[Response[T], Reference]]): Option[Either[Response[T], Reference]] =
    responses.find(x => successStatusCode(x._1)).map(_._2)

  def responsesTypes[T: Basis[JsonSchemaF, ?]]: Printer[ResponsesWithOperationId[T]] = Printer {
    case (x, y) =>
      val defaultName = TypeAliasErrorResponse(x)
      y.size match {
        case 0 => "Unit"
        case 1 => responseOrType.print(y.head._2)
        case _ =>
          val success = successType(y).fold("Unit")(responseOrType.print)
          s"Either[${defaultName.show}, $success]"
      }
  }

  def method[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      κ("  def ") *< show[OperationId],
      κ("(") *< sepBy(argumentDef, ", "),
      κ("): F[") *< responsesTypes >* κ("]")
    ).contramapN { operationTuple[T] }

  private def itemObjectTuple[T](
      thePath: String,
      itemObject: ItemObject[T],
      components: Components[T]): List[OperationWithPath[T]] = {
    val path = HttpPath.apply(thePath)

    def findParameter(name: String): Option[Parameter[T]] =
      components.parameters.get(name).flatMap(_.fold(_.some, parameterFrom))

    def parameterFrom(reference: Reference): Option[Parameter[T]] = reference.ref match {
      case parametersRegex(name) => findParameter(name)
      case name                  => findParameter(name)
    }

    def operationWithPath(verb: HttpVerb, operation: Operation[T]): OperationWithPath[T] =
      OperationWithPath(
        verb,
        path,
        operation.description,
        OperationId((verb, path, operation)),
        (itemObject.parameters ++ operation.parameters)
          .flatMap(_.fold(_.some, parameterFrom))
          .distinct,
        operation.requestBody,
        operation.responses
      )

    List(
      itemObject.get.map(operationWithPath(HttpVerb.Get, _)),
      itemObject.delete.map(operationWithPath(HttpVerb.Delete, _)),
      itemObject.post.map(operationWithPath(HttpVerb.Post, _)),
      itemObject.put.map(operationWithPath(HttpVerb.Put, _))
    ).flatten
  }

  def typeAndSchemaFor[T: Basis[JsonSchemaF, ?], X](
      operationId: Option[OperationId],
      response: Response[T],
      tpe: String)(success: => (String, List[String]))(
      implicit codecs: Printer[Codecs]): (String, Option[String], List[String]) = {
    val innerType = s"${operationId.fold("")(_.show.capitalize)}${normalize(response.description).capitalize}"
    val newSchema =
      typeFromResponse(response).map(schema(innerType.some).print).getOrElse("Unit")
    tpe match {
      case _ if (tpe === newSchema) =>
        un(second(success) { x =>
          none -> x
        })
      case _ if (newSchema.nonEmpty) => (tpe, innerType.some, List(newSchema))
      case _                         => (tpe, none, Nil)
    }
  }

  def defaultTypesAndSchemas[T: Basis[JsonSchemaF, ?]](
      operationId: OperationId,
      responseOr: Either[Response[T], Reference])(implicit codecs: Printer[Codecs]): (String, String, List[String]) = {
    def unexpectedErrorName(operationId: OperationId): String =
      s"${operationId.show}UnexpectedErrorResponse".capitalize
    val tpe     = responseOrType.print(responseOr)
    val newType = unexpectedErrorName(operationId)
    def statusCaseClass(tpe: String): String =
      caseClassDef.print(newType -> List("statusCode" -> Tpe[T]("Int"), "value" -> Tpe[T](tpe)))
    responseOr.fold(
      r => {
        val (_, anonymousType, schemas) = typeAndSchemaFor(operationId.some, r, tpe) { tpe -> List.empty }
        un(second(newType -> schemas) { x =>
          anonymousType.getOrElse(tpe) -> (x ++ List(statusCaseClass(anonymousType.getOrElse(tpe))))
        })
      },
      _ => (newType, tpe, List(statusCaseClass(tpe)))
    )
  }

  def statusTypesAndSchemas[T: Basis[JsonSchemaF, ?]](
      operationId: OperationId,
      responseOr: Either[Response[T], Reference])(
      implicit codecs: Printer[Codecs]): (String, Option[String], List[String]) = {
    val tpe = responseOrType.print(responseOr)
    responseOr.left.toOption
      .map { response =>
        def defaultResponseErrorName(operationId: OperationId, name: String): String =
          s"${operationId.show}${normalize(name).capitalize}Error".capitalize
        val (newTpe, anonymousType, schemas) = typeAndSchemaFor(operationId.some, response, tpe) {
          val newTpe = defaultResponseErrorName(operationId, response.description)
          newTpe -> List(caseClassDef.print(newTpe -> List("value" -> Tpe[T](tpe))))
        }
        (newTpe, anonymousType, schemas)
      }
      .getOrElse(
        (tpe, None, List.empty)
      )
  }

  def typesAndSchemas[T: Basis[JsonSchemaF, ?]](operationId: OperationId)(
      status: String,
      responseOr: Either[Response[T], Reference])(implicit codecs: Printer[Codecs]): (String, List[String]) = {
    val statusCodePattern = """(\d+)""".r
    val tpe               = responseOrType.print(responseOr)
    status match {
      case statusCodePattern(code) if (successStatusCode(code)) =>
        responseOr.left.toOption
          .map { response =>
            val (newTpe, _, schemas) = typeAndSchemaFor(none, response, tpe) {
              tpe -> Nil
            }
            newTpe -> schemas
          }
          .getOrElse(tpe -> List.empty)

      case statusCodePattern(_) =>
        val (x, a, y) = statusTypesAndSchemas(operationId, responseOr)
        a.getOrElse(x) -> y

      case "default" =>
        val (x, _, y) = defaultTypesAndSchemas(operationId, responseOr)
        x -> y
    }
  }

  private def responsesSchemaTuple[T: Basis[JsonSchemaF, ?]](
      operationId: OperationId,
      responses: Map[String, Either[Response[T], Reference]])(
      implicit codecs: Printer[Codecs]): Either[(List[String], TypeAliasErrorResponse, List[String]), List[String]] =
    if (responses.size > 1) {
      val (newTypes, schemas) =
        second(responses.map((typesAndSchemas[T](operationId) _).tupled).toList.unzip)(_.flatten)
      (
        schemas.toList.filter(_.nonEmpty),
        TypeAliasErrorResponse(operationId),
        newTypes.toList.filterNot(_.some === successType(responses).map(responseOrType[T].print))).asLeft
    } else
      responses.toList
        .map(_._2)
        .flatMap(r => r.left.toOption.map(_ -> responseOrType.print(r)))
        .flatMap {
          case (response, tpe) =>
            typeAndSchemaFor(none, response, tpe) { tpe -> Nil }._3
        }
        .asRight

  private def responseErrorsDef: Printer[List[String]] =
    (sepBy(string, " :+: "), (κ(" :+: CNil") >|< unit)).contramapN(errorTypes =>
      (errorTypes, if (errorTypes.size > 1) ().asLeft else ().asRight))

  private def multipleResponsesSchema: Printer[(List[String], TypeAliasErrorResponse, List[String])] = {
    import Printer.avoid._
    (
      sepBy((space >* space) *< string, "\n") >* newLine,
      space *< space *< typeAliasDef(responseErrorsDef)
    ).contramapN {
      case (schemas, tpe, errorTypes) => (schemas, (tpe.show, errorTypes, none))
    }
  }

  private def responsesSchema[T: Basis[JsonSchemaF, ?]](
      implicit codecs: Printer[Codecs]): Printer[(OperationId, Map[String, Either[Response[T], Reference]])] =
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

  private def requestSchema[T: Basis[JsonSchemaF, ?]](
      implicit codecs: Printer[Codecs]): Printer[(OperationId, Option[Either[Request[T], Reference]])] =
    (optional((space >* space) *< schemaWithName[T])).contramap((requestSchemaTuple[T] _).tupled)

  private def clientTypes[T: Basis[JsonSchemaF, ?]](
      implicit codecs: Printer[Codecs]): Printer[List[OperationWithPath[T]]] =
    (sepBy(requestSchema[T], "\n") >* newLine, sepBy(responsesSchema[T], "\n") >* newLine)
      .contramapN {
        _.map { operation =>
          (operation.operationId   -> operation.requestBody) ->
            (operation.operationId -> operation.responses)
        }.unzip
      }

  def toOperationsWithPath[T](
      traitName: TraitName,
      itemObjects: Map[String, ItemObject[T]],
      components: Components[T]): (TraitName, List[OperationWithPath[T]]) =
    traitName -> itemObjects.toList.flatMap { case (n, i) => itemObjectTuple[T](n, i, components) }

  private def operationsTuple[T: Basis[JsonSchemaF, ?]](openApi: OpenApi[T]): (
      List[PackageName],
      TraitName,
      TraitName,
      List[OperationWithPath[T]],
      (String, List[PackageName], List[OperationWithPath[T]])) = {
    val traitName = TraitName(openApi)

    val (a, b, c, d) = un(second(duplicate(toOperationsWithPath(traitName, openApi.paths, componentsFrom(openApi)))) {
      case (a, b) => (a, (traitName, b))
    })
    (List("models._", "shapeless.{:+:, CNil}").map(PackageName.apply), a, b, c, (d._1.show, List.empty, d._2))
  }

  def interfaceDefinition[T: Basis[JsonSchemaF, ?]](implicit codecs: Printer[Codecs]): Printer[OpenApi[T]] =
    (
      sepBy(importDef, "\n") >* newLine,
      κ("trait ") *< show[TraitName] >* κ("[F[_]] {") >* newLine,
      space *< space *< κ("import ") *< show[TraitName] >* κ("._") >* newLine,
      sepBy(method[T], "\n") >* (newLine >* κ("}") >* newLine),
      objectDef(clientTypes[T])
    ).contramapN(operationsTuple[T])

}
