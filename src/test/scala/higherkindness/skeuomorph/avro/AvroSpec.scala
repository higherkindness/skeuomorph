/*
 * Copyright 2018-2020 47 Degrees Open Source <https://www.47deg.com>
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

package higherkindness.skeuomorph.avro

import higherkindness.skeuomorph.instances._

import org.apache.avro.Schema
import org.apache.avro.compiler.idl._
import org.scalacheck._
import org.specs2._
import higherkindness.skeuomorph.mu.{Protocol => MuProtocol}
import higherkindness.skeuomorph.mu.CompressionType
import higherkindness.skeuomorph.mu.codegen
import higherkindness.droste._
import higherkindness.droste.data.Mu
import scala.meta._
import scala.meta.contrib._

import scala.jdk.CollectionConverters._

class AvroSpec extends Specification with ScalaCheck {

  def is = s2"""
  Avro Schema

  It should be possible to create a Schema from org.apache.avro.Schema. $convertSchema

  It should be possible to create a Protocol from org.apache.avro.Protocol and then generate Scala code from it. $convertAndPrintProtocol
  """

  def convertSchema =
    Prop.forAll { (schema: Schema) =>
      val test = scheme.hylo(checkSchema(schema), AvroF.fromAvro)

      test(schema)
    }

  def checkSchema(sch: Schema): Algebra[AvroF, Boolean] =
    Algebra {
      case AvroF.TNull()    => sch.getType should_== Schema.Type.NULL
      case AvroF.TBoolean() => sch.getType should_== Schema.Type.BOOLEAN
      case AvroF.TInt()     => sch.getType should_== Schema.Type.INT
      case AvroF.TLong()    => sch.getType should_== Schema.Type.LONG
      case AvroF.TFloat()   => sch.getType should_== Schema.Type.FLOAT
      case AvroF.TDouble()  => sch.getType should_== Schema.Type.DOUBLE
      case AvroF.TBytes()   => sch.getType should_== Schema.Type.BYTES
      case AvroF.TString()  => sch.getType should_== Schema.Type.STRING

      case AvroF.TNamedType(_, _) => false
      case AvroF.TArray(_)        => sch.getType should_== Schema.Type.ARRAY
      case AvroF.TMap(_)          => sch.getType should_== Schema.Type.MAP
      case AvroF.TRecord(name, namespace, _, doc, fields) =>
        (sch.getName should_== name)
          .and(sch.getNamespace should_== namespace.getOrElse(""))
          .and(sch.getDoc should_== doc.getOrElse(""))
          .and(
            sch.getFields.asScala.toList.map(f => (f.name, f.doc)) should_== fields
              .map(f => (f.name, f.doc.getOrElse("")))
          )

      case AvroF.TEnum(_, _, _, _, _) => true
      case AvroF.TUnion(_)            => true
      case AvroF.TFixed(_, _, _, _)   => true
    }

  def convertAndPrintProtocol = {
    val idl       = new Idl(getClass.getClassLoader.getResourceAsStream("avro/GreeterService.avdl"))
    val avroProto = idl.CompilationUnit()

    val skeuoAvroProto = Protocol.fromProto[Mu[AvroF]](avroProto)

    val muProto = MuProtocol.fromAvroProtocol(CompressionType.Identity, useIdiomaticEndpoints = true)(skeuoAvroProto)

    val streamCtor: (Type, Type) => Type.Apply = { case (f: Type, a: Type) =>
      t"Stream[$f, $a]"
    }

    val actual = codegen.protocol(muProto, streamCtor).right.get

    val expected = codegenExpectation(CompressionType.Identity, useIdiomaticEndpoints = true)
      .parse[Source]
      .get
      .children
      .head
      .asInstanceOf[Pkg]

    actual.isEqual(expected) :| s"""
      |Actual output:
      |$actual
      |
      |
      |Expected output:
      |$expected"
      """.stripMargin
  }

  // TODO test for more complex schemas, importing other files, etc.

  def codegenExpectation(compressionType: CompressionType, useIdiomaticEndpoints: Boolean): String = {

    val serviceParams: String = "Avro" +
      (if (compressionType == CompressionType.Gzip) ", Gzip" else ", Identity") +
      (if (useIdiomaticEndpoints) ", namespace = Some(\"foo.bar\"), methodNameStyle = Capitalize" else "")

    s"""package foo.bar
      |
      |import _root_.higherkindness.mu.rpc.protocol._
      |final case class LogicalIdl(dec: _root_.shapeless.tag.@@[_root_.scala.math.BigDecimal, (("precision", 20), ("scale", 8))], maybeDec: _root_.scala.Option[_root_.shapeless.tag.@@[_root_.scala.math.BigDecimal, (("precision", 20), ("scale", 8))]], ts: _root_.java.time.Instant, dt: _root_.java.time.LocalDate)
      |final case class HelloRequest(
      |  arg1: _root_.java.lang.String,
      |  arg2: _root_.scala.Option[_root_.java.lang.String],
      |  arg3: _root_.scala.List[_root_.java.lang.String]
      |)
      |final case class HelloResponse(
      |  arg1: _root_.java.lang.String,
      |  arg2: _root_.scala.Option[_root_.java.lang.String],
      |  arg3: _root_.scala.List[_root_.java.lang.String]
      |)
      |
      |@service($serviceParams) trait MyGreeterService[F[_]] {
      |  def sayHelloAvro(req: _root_.foo.bar.HelloRequest): F[_root_.foo.bar.HelloResponse]
      |  def sayNothingAvro(req: _root_.higherkindness.mu.rpc.protocol.Empty.type): F[_root_.higherkindness.mu.rpc.protocol.Empty.type]
      |}
      """.stripMargin
  }

}
