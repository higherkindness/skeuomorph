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

package higherkindness.skeuomorph.protobuf

import cats.effect.IO
import higherkindness.skeuomorph.mu.MuF
import higherkindness.skeuomorph.protobuf.ProtobufF._
import higherkindness.skeuomorph.protobuf.ParseProto._
import higherkindness.skeuomorph.mu.CompressionType
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}
import higherkindness.droste.data.Mu
import higherkindness.droste.data.Mu._

class ProtobufProtocolSpec extends Specification with ScalaCheck {

  val currentDirectory: String                  = new java.io.File(".").getCanonicalPath
  val root                                      = "/src/test/scala/higherkindness/skeuomorph/protobuf"
  val path                                      = currentDirectory + s"$root/service"
  val importRoot                                = Some(currentDirectory + root)
  val source                                    = ProtoSource(s"book.proto", path, importRoot)
  val protobufProtocol: Protocol[Mu[ProtobufF]] = parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()

  implicit val arbCompressionType: Arbitrary[CompressionType] = Arbitrary {
    Gen.oneOf(CompressionType.Identity, CompressionType.Gzip)
  }

  def is = s2"""
  Protobuf Protocol

  It should be possible to print a protocol from a Proto file. $printProtobufProtocol

  """

  def printProtobufProtocol = prop { (ct: CompressionType, useIdiom: Boolean) =>
    val parseProtocol: Protocol[Mu[ProtobufF]] => higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] = {
      p: Protocol[Mu[ProtobufF]] =>
        higherkindness.skeuomorph.mu.Protocol.fromProtobufProto(ct, useIdiom)(p)
    }

    val printProtocol: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] => String = {
      p: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] =>
        higherkindness.skeuomorph.mu.print.proto.print(p)
    }

    val actual   = (parseProtocol andThen printProtocol)(protobufProtocol)
    val expected = expectation(ct, useIdiom)

    (actual.clean must beEqualTo(expected.clean)) :| s"""
      |Actual output:
      |$actual
      |
      |
      |Expected output:
      |$expected"
      """.stripMargin
  }

  def expectation(compressionType: CompressionType, useIdiomaticEndpoints: Boolean): String = {

    val serviceParams: String = "Protobuf" +
      (if (compressionType == CompressionType.Gzip) ", Gzip" else ", Identity") +
      (if (useIdiomaticEndpoints) ", namespace = Some(\"com.acme\"), methodNameStyle = Capitalize" else "")

    s"""package com.acme
      |
      |object book {
      |
      |@message final case class Book(isbn: _root_.scala.Long, title: _root_.java.lang.String, author: _root_.scala.List[_root_.scala.Option[_root_.com.acme.author.Author]], binding_type: _root_.scala.Option[_root_.com.acme.book.BindingType], rating: _root_.scala.Option[_root_.com.acme.rating.Rating], `private`: _root_.scala.Boolean, `type`: _root_.scala.Option[_root_.com.acme.book.`type`])
      |@message final case class `type`(foo: _root_.scala.Long, thing: _root_.scala.Option[_root_.com.acme.`hyphenated-name`.Thing])
      |@message final case class GetBookRequest(isbn: _root_.scala.Long)
      |@message final case class GetBookViaAuthor(author: _root_.scala.Option[_root_.com.acme.author.Author])
      |@message final case class BookStore(name: _root_.java.lang.String, books: _root_.scala.Map[_root_.scala.Long, _root_.java.lang.String], genres: _root_.scala.List[_root_.scala.Option[_root_.com.acme.book.Genre]], payment_method: Cop[_root_.scala.Long :: _root_.scala.Int :: _root_.java.lang.String :: _root_.com.acme.book.Book :: TNil])
      |
      |sealed trait Genre
      |object Genre {
      |  case object UNKNOWN extends Genre
      |  case object SCIENCE_FICTION extends Genre
      |  case object POETRY extends Genre
      |}
      |
      |sealed trait BindingType
      |object BindingType {
      |  case object HARDCOVER extends BindingType
      |  case object PAPERBACK extends BindingType
      |}
      |
      |@service($serviceParams) trait BookService[F[_]] {
      |  def GetBook(req: _root_.com.acme.book.GetBookRequest): F[_root_.com.acme.book.Book]
      |  def GetBooksViaAuthor(req: _root_.com.acme.book.GetBookViaAuthor): Stream[F, _root_.com.acme.book.Book]
      |  def GetGreatestBook(req: Stream[F, _root_.com.acme.book.GetBookRequest]): F[_root_.com.acme.book.Book]
      |  def GetBooks(req: Stream[F, _root_.com.acme.book.GetBookRequest]): Stream[F, _root_.com.acme.book.Book]
      |  def GetRatingOfAuthor(req: _root_.com.acme.author.Author): F[_root_.com.acme.rating.Rating]
      |}
      |
      |}""".stripMargin
  }

  implicit class StringOps(self: String) {
    def clean: String = self.replaceAll("\\s", "")
  }

}
