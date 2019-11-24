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

  val currentDirectory: String = new java.io.File(".").getCanonicalPath
  val root                     = "/src/test/scala/higherkindness/skeuomorph/protobuf"
  val path                     = currentDirectory + s"$root/service"
  val importRoot               = Some(currentDirectory + root)

  implicit val arbCompressionType: Arbitrary[CompressionType] = Arbitrary {
    Gen.oneOf(CompressionType.Identity, CompressionType.Gzip)
  }

  def is = s2"""
  Protobuf Protocol
  It prints models from a models Proto file. ${printModelProtobufProtocol("book")}
  It prints a service from a service Proto file. ${printServiceProtobufProtocol("bookService")}
  """

  def parseProtobufProtocol(fileName: String)(ct: CompressionType, useIdiom: Boolean) = {
    val source                                    = ProtoSource(s"$fileName.proto", path, importRoot)
    val protobufProtocol: Protocol[Mu[ProtobufF]] = parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()

    val parseProtocol: Protocol[Mu[ProtobufF]] => higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] = {
      p: Protocol[Mu[ProtobufF]] =>
        higherkindness.skeuomorph.mu.Protocol.fromProtobufProto(ct, useIdiom)(p)
    }

    val printProtocol: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] => String = {
      p: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] =>
        higherkindness.skeuomorph.mu.print.proto.print(p)
    }

    (parseProtocol andThen printProtocol)(protobufProtocol)
  }

  def printModelProtobufProtocol(fileName: String) = prop { (ct: CompressionType, useIdiom: Boolean) =>
    val result = parseProtobufProtocol(fileName)(ct, useIdiom)
    println(s"\n${result}\n")

    result.clean must beEqualTo(modelsExpectation.clean)
  }

  def printServiceProtobufProtocol(fileName: String) = prop { (ct: CompressionType, useIdiom: Boolean) =>
    val result = parseProtobufProtocol(fileName)(ct, useIdiom)

    result.clean must beEqualTo(serviceExpectation(ct, useIdiom).clean)
  }

  val modelsExpectation: String =
    """package com.acme
      |
      |import com.acme.author.Author
      |import com.acme.rating.Rating
      |
      |object book {
      |
      |  sealed trait Genre
      |  object Genre {
      |    case object UNKNOWN extends Genre
      |    case object SCIENCE_FICTION extends Genre
      |    case object POETRY extends Genre
      |  }
      |
      |  sealed trait BindingType
      |  object BindingType {
      |    case object HARDCOVER extends BindingType
      |    case object PAPERBACK extends BindingType
      |  }
      |
      |  @message final case class Book(isbn: Long, title: String, author: List[Author], binding_type: BindingType, rating: Rating)
      |
      |  @message final case class BookSeries(isbn: Long)
      |
      |  @message final case class Series(code: Long, name: String, numberOfBooks: Int, date: String, books: List[BookSeries])
      |
      |  @message final case class GetBookRequest(isbn: Long)
      |
      |  @message final case class GetBookViaAuthor(author: Author)
      |
      |  @message final case class GetBookSeries(isbn: Long)
      |
      |  @message final case class BookStore(name: String, books: Map[Long, String], genres: List[Genre], payment_method: Cop[Long :: Int :: String :: Book :: TNil])
      |
      |}
      |""".stripMargin

  def serviceExpectation(compressionType: CompressionType, useIdiomaticEndpoints: Boolean): String = {
    val params: String =
      "Protobuf" +
        (if (compressionType == CompressionType.Gzip) ", Gzip" else ", Identity") +
        (if (useIdiomaticEndpoints) ", namespace = Some(\"com.acme\"), methodNameStyle = Capitalize" else "")

    s"""package com.acme
       |
       |import com.acme.book.Book
       |import com.acme.book.Series
       |import com.acme.book.GetBookRequest
       |import com.acme.book.GetBookViaAuthor
       |import com.acme.book.GetBookSeries
       |import com.acme.book.BookStore
       |
       |object bookService {
       |
       |  @service($params) trait BookService[F[_]] {
       |    def GetBook(req: GetBookRequest): F[Book]
       |    def GetBooksSeries(req: GetBookSeries): F[Series]
       |    def GetBooksViaAuthor(req: GetBookViaAuthor): Stream[F, Book]
       |    def GetGreatestBook(req: Stream[F, GetBookRequest]): F[Book]
       |    def GetBooks(req: Stream[F, GetBookRequest]): Stream[F, Book]
       |  }
       |
       |}""".stripMargin
  }

  implicit class StringOps(self: String) {
    def clean: String = self.replaceAll("\\s", "")
  }

}
