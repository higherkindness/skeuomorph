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

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer._
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.uast.types.{string => _, _}

import higherkindness.skeuomorph.mu
import higherkindness.skeuomorph.mu.Optimize.namedTypes
import higherkindness.skeuomorph.mu.SerializationType._

import cats.implicits._

import higherkindness.droste._

object print {

  def schema[T: Project[mu.Type, ?]]: Printer[T] = {
    val algebra: Algebra[mu.Type, String] = Algebra {
      case mu.InjNull(TNull())                      => "Null"
      case mu.InjDouble(TDouble())                  => "Double"
      case mu.InjFloat(TFloat())                    => "Float"
      case mu.InjInt(TInt())                        => "Int"
      case mu.InjLong(TLong())                      => "Long"
      case mu.InjBoolean(TBoolean())                => "Boolean"
      case mu.InjString(TString())                  => "String"
      case mu.InjByteArray(TByteArray())            => "Array[Byte]"
      case mu.InjOption(TOption(value))             => s"Option[$value]"
      case mu.InjEither(TEither(a, b))              => s"Either[$a, $b]"
      case mu.InjMap(TMap(key, value))              => s"Map[$key, $value]"
      case mu.InjGeneric(TGeneric(generic, params)) => s"""$generic[${params.mkString(", ")}]"""
      case mu.InjList(TList(value))                 => s"List[$value]"
      case mu.InjUnion(TUnion(invariants)) =>
        invariants.toList.mkString("Cop[", " :: ", ":: TNil]")
      case mu.InjEnum(TEnum(name, fields)) =>
        val printFields = fields.map(f => s"case object $f extends $name").mkString("\n  ")
        s"""
      |sealed trait $name
      |object $name {
      |  $printFields
      |}
      """.stripMargin
      case mu.InjRecord(TRecord(name, fields)) =>
        val printFields = fields.map(f => s"${FieldF.fieldName.get(f)}: ${FieldF.fieldType.get(f)}").mkString(", ")
        s"@message final case class $name($printFields)"
    }

    Printer(scheme.cata(algebra))
  }

  /**
   * Needed to be able to use the Protocol case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def protoTuple[T](
      proto: Protocol[T]
  ): (Option[String], List[(String, String)], String, List[T], List[Service[T]]) =
    proto match {
      case Protocol(name, pkg, options, declarations, services) =>
        (pkg, options, name, declarations, services)
    }

  /**
   * Needed to be able to use the Service.Operation case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def opTuple[T](
      op: Service.Operation[T]
  ): (String, T, T) =
    op match {
      case Service.Operation(name, Service.OperationType(request, _), Service.OperationType(response, _)) =>
        (name, request, response)
    }

  /**
   * Needed to be able to use the Service case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def serviceTuple[T](
      s: Service[T]
  ): (SerializationType, String, List[Service.Operation[T]]) =
    s match {
      case Service(name, serType, ops) =>
        (serType, name, ops)
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

  def protobuf: Printer[Protobuf.type]             = Printer(_.toString)
  def avro: Printer[Avro.type]                     = Printer(_.toString)
  def avroWithSchema: Printer[AvroWithSchema.type] = Printer(_.toString)

  def serializationType: Printer[SerializationType] =
    (protobuf >|< avro >|< avroWithSchema).contramap(serTypeEither)

  def operation[T](implicit T: Basis[mu.Type, T]): Printer[Service.Operation[T]] =
    (
      (konst("def ") *< string),
      (konst("(req: ") *< Printer(namedTypes[mu.Type, T] >>> schema.print)),
      (konst("): ") *< Printer(namedTypes[mu.Type, T] >>> schema.print))
    ).contramapN(opTuple)

  def service[T](implicit T: Basis[mu.Type, T]): Printer[Service[T]] =
    (
      (konst("@service(") *< serializationType >* konst(") trait ")),
      (string >* konst("[F[_]] {") >* newLine),
      (sepBy(operation, "\n") >* newLine >* konst("}"))
    ).contramapN(serviceTuple)

  def option: Printer[(String, String)] =
    (konst("@option(name = ") *< string) >*< (konst(", value = ") *< string >* konst(")"))

  def proto[T](implicit T: Basis[mu.Type, T]): Printer[Protocol[T]] = {
    val lineFeed       = "\n"
    val doubleLineFeed = "\n\n "
    (
      (konst("package ") *< optional(string) >* newLine >* newLine),
      sepBy(option, lineFeed),
      (konst("object ") *< string >* konst(" { ") >* newLine >* newLine),
      (sepBy(schema, lineFeed) >* newLine),
      (sepBy(service, doubleLineFeed) >* (newLine >* newLine >* konst("}")))
    ).contramapN(protoTuple)
  }
}
