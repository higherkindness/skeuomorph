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
package mu

import Optimize.namedTypes
import cats.instances.function._
import cats.syntax.compose._

import qq.droste._
import MuF._
import catz.contrib.ContravariantMonoidalSyntax._
import catz.contrib.Decidable._
import Printer._
import SerializationType._

object print {

  def schema[T: Basis[MuF, ?]]: Printer[T] = {
    val algebra: Algebra[MuF, String] = Algebra {
      case TNull()                   => "Null"
      case TDouble()                 => "Double"
      case TFloat()                  => "Float"
      case TInt()                    => "Int"
      case TLong()                   => "Long"
      case TBoolean()                => "Boolean"
      case TString()                 => "String"
      case TByteArray()              => "Array[Byte]"
      case TNamedType(name)          => name
      case TOption(value)            => s"Option[$value]"
      case TEither(a, b)             => s"Either[$a, $b]"
      case TMap(value)               => s"Map[String, $value]"
      case TGeneric(generic, params) => s"""$generic[${params.mkString(", ")}]"""
      case TList(value)              => s"List[$value]"
      case TRequired(value)          => value
      case TCoproduct(invariants) =>
        invariants.toList.mkString("Cop[", " :: ", ":: TNil]")
      case TSum(name, fields) =>
        val printFields = fields.map(f => s"case object $f extends $name").mkString("\n  ")
        s"""
      |sealed trait $name
      |object $name {
      |  $printFields
      |}
      """.stripMargin
      case TProduct(name, fields) =>
        val printFields = fields.map(f => s"${f.name}: ${f.tpe}").mkString(", ")
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
  ): ((((Option[String], List[(String, String)]), String), List[T]), List[skeuomorph.mu.Service[T]]) =
    proto match {
      case Protocol(name, pkg, options, declarations, services) =>
        ((((pkg, options), name), declarations), services)
    }

  /**
   * Needed to be able to use the Service.Operation case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def opTuple[T](
      op: Service.Operation[T]
  ): ((String, T), T) =
    op match {
      case Service.Operation(name, request, response) =>
        ((name, request), response)
    }

  /**
   * Needed to be able to use the Service case class
   * as a [[cats.ContravariantMonoidal]].
   */
  def serviceTuple[T](
      s: Service[T]
  ): ((SerializationType, String), List[Service.Operation[T]]) =
    s match {
      case Service(name, serType, ops) =>
        ((serType, name), ops)
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

  def operation[T](implicit T: Basis[MuF, T]): Printer[Service.Operation[T]] =
    ((konst("def ") *< string) >*< (konst("(req: ") *< Printer(namedTypes[T] >>> schema.print)) >*< (konst("): ") *< Printer(
      namedTypes[T] >>> schema.print)))
      .contramap(opTuple)

  def service[T](implicit T: Basis[MuF, T]): Printer[Service[T]] =
    ((konst("@service(") *< serializationType >* konst(") trait ")) >*<
      (string >* konst("[F[_]] {") >* newLine) >*<
      (sepBy(operation, "\n") >* newLine >* konst("}"))).contramap(serviceTuple)

  def option: Printer[(String, String)] =
    (konst("@option(name = ") *< string) >*< (konst(", value = ") *< string >* konst(")"))

  def proto[T](implicit T: Basis[MuF, T]): Printer[Protocol[T]] = {
    val lineFeed       = "\n"
    val doubleLineFeed = "\n\n "
    ((konst("package ") *< optional(string) >* (newLine >* newLine)) >*<
      sepBy(option, lineFeed) >*<
      (konst("object ") *< string >* konst(" { ") >* newLine >* newLine) >*<
      (sepBy(schema, lineFeed) >* newLine) >*<
      (sepBy(service, doubleLineFeed) >* (newLine >* newLine >* konst("}")))).contramap(protoTuple)
  }
}
