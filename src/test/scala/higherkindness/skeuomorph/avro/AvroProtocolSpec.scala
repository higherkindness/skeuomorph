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

import higherkindness.droste.data.Mu
import higherkindness.skeuomorph.mu.{CompressionType, SerializationType, codegen, Protocol => MuProtocol}
import org.apache.avro.compiler.idl._
import org.scalacheck._
import org.specs2._
import scala.meta._
import scala.meta.contrib._

class AvroProtocolSpec extends Specification with ScalaCheck {

  implicit val arbCompressionType: Arbitrary[CompressionType] = Arbitrary {
    Gen.oneOf(CompressionType.Identity, CompressionType.Gzip)
  }

  def is = s2"""
  Avro Protocol

  It should be possible to create a Protocol from org.apache.avro.Protocol and then generate Scala code from it. $codegenAvroProtocol
  """

  def codegenAvroProtocol =
    prop { (compressionType: CompressionType, useIdiomaticGrpc: Boolean, useIdiomaticScala: Boolean) =>

      val idl       = new Idl(getClass.getClassLoader.getResourceAsStream("avro/GreeterService.avdl"))
      val avroProto = idl.CompilationUnit()

      val skeuoAvroProto = Protocol.fromProto[Mu[AvroF]](avroProto)

      val muProto = MuProtocol.fromAvroProtocol(compressionType, useIdiomaticGrpc, useIdiomaticScala)(skeuoAvroProto)

      val streamCtor: (Type, Type) => Type.Apply = { case (f: Type, a: Type) =>
        t"Stream[$f, $a]"
      }

      val actual = codegen.protocol(muProto, streamCtor).right.get

      val expected = codegenExpectation(compressionType, muProto.pkg, useIdiomaticGrpc, useIdiomaticScala)
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

  private def codegenExpectation(compressionType: CompressionType, namespace: Option[String],
      useIdiomaticGrpc: Boolean, useIdiomaticScala: Boolean): String = {

    val serviceParams: String = Seq(SerializationType.Avro,
      s"compressionType = $compressionType",
      s"methodNameStyle = Unchanged", // IDL services are not capitalized
      s"namespace = ${if (useIdiomaticGrpc) namespace.map("\"" + _ + "\"") else None}",
    ).mkString(", ")

    s"""package com.acme
      |
      |import _root_.higherkindness.mu.rpc.protocol._
      |
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
      |  def sayHelloAvro(req: _root_.com.acme.HelloRequest): F[_root_.com.acme.HelloResponse]
      |  def sayNothingAvro(req: _root_.higherkindness.mu.rpc.protocol.Empty.type): F[_root_.higherkindness.mu.rpc.protocol.Empty.type]
      |}
      """.stripMargin
  }

}
