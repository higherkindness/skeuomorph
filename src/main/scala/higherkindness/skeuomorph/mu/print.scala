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

package higherkindness.skeuomorph.mu

import cats.implicits._
import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer._
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.mu.CompressionType.{Gzip, Identity}
import higherkindness.skeuomorph.mu.MuF.{string => _, _}
import higherkindness.skeuomorph.mu.Optimize.namedTypes
import higherkindness.skeuomorph.mu.Optimize.nestedOptionInCoproduct
import higherkindness.skeuomorph.mu.SerializationType._
import higherkindness.droste._

object print {

  def schema[T: Project[MuF, ?]]: Printer[T] = {
    def printIdentifier(prefix: List[String], name: String): String = {
      if (prefix.isEmpty)
        toValidIdentifier(name)
      else
        s"_root_.${prefix.map(toValidIdentifier).mkString(".")}.${toValidIdentifier(name)}"
    }

    val algebra: Algebra[MuF, String] = Algebra {
      case TNull()                   => "Null"
      case TDouble()                 => "_root_.scala.Double"
      case TFloat()                  => "_root_.scala.Float"
      case TInt()                    => "_root_.scala.Int"
      case TLong()                   => "_root_.scala.Long"
      case TBoolean()                => "_root_.scala.Boolean"
      case TString()                 => "_root_.java.lang.String"
      case TByteArray()              => "_root_.scala.Array[Byte]"
      case TNamedType(prefix, name)  => printIdentifier(prefix, name)
      case TOption(value)            => s"_root_.scala.Option[$value]"
      case TEither(a, b)             => s"_root_.scala.Either[$a, $b]"
      case TMap(Some(key), value)    => s"_root_.scala.Map[$key, $value]"
      case TMap(None, value)         => s"_root_.scala.Map[String, $value]" // Compatibility for Avro
      case TGeneric(generic, params) => s"""$generic[${params.mkString(", ")}]"""
      case TList(value)              => s"_root_.scala.List[$value]"
      case TContaining(values)       => values.mkString("\n")
      case TRequired(value)          => value
      case TCoproduct(invariants) =>
        invariants.toList.mkString("Cop[", " :: ", " :: TNil]")
      case TSum(name, fields) =>
        val printFields = fields.map(f => s"case object ${f.name} extends ${name}(${f.value})").mkString("\n  ")
        s"""
      |sealed abstract class $name(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
      |object $name extends _root_.enumeratum.values.IntEnum[$name] {
      |  $printFields
      |
      |  val values = findValues
      |}
      """.stripMargin
      case TProduct(name, fields) =>
        def printField(f: Field[_]) =
          s"@_root_.pbdirect.pbIndex(${f.indices.mkString(",")}) ${toValidIdentifier(f.name)}: ${f.tpe}"
        val printFields = fields.map(printField).mkString(",\n  ")
        s"""|@message final case class ${toValidIdentifier(name)}(
            |  $printFields
            |)""".stripMargin
    }

    Printer.print(scheme.cata(algebra))
  }

  /**
   * Needed to be able to use the Protocol case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def protoTuple[T](
      proto: Protocol[T]
  ): (Option[String], List[(String, String)], List[DependentImport[T]], String, List[T], List[Service[T]]) =
    proto match {
      case Protocol(name, pkg, options, declarations, services, imports) =>
        (pkg, options, imports, name, declarations, services)
    }

  /**
   * Needed to be able to use the Service.Operation case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def opTuple[T](
      op: Service.Operation[T]
  ): (String, Service.OperationType[T], Service.OperationType[T]) =
    op match {
      case Service.Operation(name, request, response) =>
        (name, request, response)
    }

  /**
   * Needed to be able to use the Service case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def serviceTuple[T](
      s: Service[T]
  ): (SerializationType, CompressionType, IdiomaticEndpoints, String, List[Service.Operation[T]]) =
    s match {
      case Service(name, serType, compType, idiomEndpoints, ops) =>
        (serType, compType, idiomEndpoints, name, ops)
    }

  /**
   * Needed to be able to use the DependentImport case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def importTuple[T](i: DependentImport[T]): (String, String, T) = i match {
    case DependentImport(pkg, name, tpe) => (pkg, name, tpe)
  }

  /**
   * needed to use SerializationType as a [[catz.contrib.Decidable]].
   */
  def serTypeEither(serType: SerializationType): Either[Either[Protobuf.type, Avro.type], AvroWithSchema.type] =
    serType match {
      case Protobuf       => Left(Left(Protobuf))
      case Avro           => Left(Right(Avro))
      case AvroWithSchema => Right(AvroWithSchema)
    }

  def protobuf: Printer[Protobuf.type]             = Printer.print(_.toString)
  def avro: Printer[Avro.type]                     = Printer.print(_.toString)
  def avroWithSchema: Printer[AvroWithSchema.type] = Printer.print(_.toString)

  def serializationType: Printer[SerializationType] =
    (protobuf >|< avro >|< avroWithSchema).contramap(serTypeEither)

  def gzip: Printer[Gzip.type]         = Printer.print(_.toString)
  def identity: Printer[Identity.type] = Printer.print(_.toString)

  def compressionType: Printer[CompressionType] =
    (gzip >|< identity).contramap({
      case Gzip     => Left(Gzip)
      case Identity => Right(Identity)
    })

  def idiomaticEndpoints: Printer[IdiomaticEndpoints] =
    Printer {
      case IdiomaticEndpoints(Some(pkg), true) => ", namespace = Some(\"" + pkg + "\"), methodNameStyle = Capitalize"
      case IdiomaticEndpoints(None, true)      => ", methodNameStyle = Capitalize\""
      case _                                   => ""
    }

  def opTpeEither[T](op: Service.OperationType[T], isRequest: Boolean): Either[Either[Either[T, T], T], T] =
    (op.stream, isRequest) match {
      case (false, true)  => Left(Left(Left(op.tpe)))
      case (true, true)   => Left(Left(Right(op.tpe)))
      case (false, false) => Left(Right(op.tpe))
      case (true, false)  => Right(op.tpe)
    }

  def opTpe[T](isRequest: Boolean)(implicit T: Basis[MuF, T]): Printer[Service.OperationType[T]] =
    (opTypeRequestNoStream >|< opTypeStream >|< opTypeResponseNoStream >|< opTypeStream)
      .contramap(t => opTpeEither(t, isRequest))

  def opTypeRequestNoStream[T](implicit T: Basis[MuF, T]): Printer[T] =
    Printer.print(namedTypes[T] >>> schema.print)

  def opTypeResponseNoStream[T](implicit T: Basis[MuF, T]): Printer[T] =
    konst("F[") *< Printer.print(namedTypes[T] >>> schema.print) >* konst("]")

  def opTypeStream[T](implicit T: Basis[MuF, T]): Printer[T] =
    konst("Stream[F, ") *< Printer.print(namedTypes[T] >>> schema.print) >* konst("]")

  def operation[T](implicit T: Basis[MuF, T]): Printer[Service.Operation[T]] =
    (
      konst("  def ") *< string,
      konst("(req: ") *< opTpe(true),
      konst("): ") *< opTpe(false)
    ).contramapN(opTuple)

  def service[T](implicit T: Basis[MuF, T]): Printer[Service[T]] =
    (
      konst("@service(") *< serializationType,
      konst(", ") *< compressionType,
      idiomaticEndpoints >* konst(") trait "),
      string >* konst("[F[_]] {") >* newLine,
      sepBy(operation, "\n") >* newLine >* konst("}")
    ).contramapN(serviceTuple)

  def depImport[T](implicit T: Basis[MuF, T]): Printer[DependentImport[T]] =
    (
      konst("import ") *< string,
      konst(".") *< identifier,
      konst(".") *< Printer.print(namedTypes[T] >>> schema.print)
    ).contramapN(importTuple)

  def option: Printer[(String, String)] =
    (konst("@option(name = ") *< string) >*< (konst(", value = ") *< string >* konst(")"))

  def proto[T](implicit T: Basis[MuF, T]): Printer[Protocol[T]] = {
    val lineFeed       = "\n"
    val doubleLineFeed = "\n\n "
    (
      konst("package ") *< optional(string) >* newLine >* newLine,
      sepBy(option, lineFeed),
      sepBy(depImport, lineFeed) >* newLine >* newLine,
      konst("object ") *< identifier >* konst(" { ") >* newLine >* newLine,
      sepBy(Printer.print(nestedOptionInCoproduct[T] >>> schema.print), lineFeed) >* newLine,
      sepBy(service, doubleLineFeed) >* (newLine >* newLine >* konst("}"))
    ).contramapN(protoTuple)
  }
}
