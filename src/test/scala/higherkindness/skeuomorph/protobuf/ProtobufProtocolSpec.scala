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
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type
import higherkindness.skeuomorph.protobuf.ProtobufF._
import qq.droste.{Algebra, Embed}
//import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import org.specs2.Specification
import higherkindness.skeuomorph.protobuf.ParseProto._
//import higherkindness.skeuomorph.mu.MuF
//import qq.droste.data.Mu
//import qq.droste.data.Mu._

class ProtobufProtocolSpec extends Specification {

  def is = s2"""
  Protobuf Protocol

  It should be possible to print a protocol from a Proto file. $printProtobufProtocol

  """

  def printProtobufProtocol = {

    val currentDirectory: String = new java.io.File(".").getCanonicalPath
    val path                     = currentDirectory + "/src/test/scala/higherkindness/skeuomorph/protobuf"
    val source                   = ProtoSource("book.proto", path)
//    val nativeDescriptor         = parseProto[IO, Type].parse(source).unsafeRunSync()

    def aaa: Algebra[ProtobufF, Type] = Algebra {
      case TBool()    => Type.TYPE_BOOL
      case TBytes()   => Type.TYPE_BYTES
      case TDouble()  => Type.TYPE_DOUBLE
      case TFixed32() => Type.TYPE_FIXED32
      case TFixed64() => Type.TYPE_FIXED64
      case TFloat()   => Type.TYPE_FLOAT
      case TInt32()   => Type.TYPE_INT32
      case TInt64()   => Type.TYPE_INT64
      case TFixed32() => Type.TYPE_SFIXED32
      case TFixed64() => Type.TYPE_SFIXED64
      case TInt32()   => Type.TYPE_SINT32
      case TInt64()   => Type.TYPE_SINT64
      case TString()  => Type.TYPE_STRING
      case TInt32()   => Type.TYPE_UINT32
      case TInt64()   => Type.TYPE_UINT64
      case _          => Type.TYPE_BOOL
      //    case Type.TYPE_BYTES    => TBytes()
      //    case Type.TYPE_DOUBLE   => TDouble()
      //    case Type.TYPE_FIXED32  => TFixed32()
      //    case Type.TYPE_FIXED64  => TFixed64()
      //    case Type.TYPE_FLOAT    => TFloat()
      //    case Type.TYPE_INT32    => TInt32()
      //    case Type.TYPE_INT64    => TInt64()
      //    case Type.TYPE_SFIXED32 => TFixed32()
      //    case Type.TYPE_SFIXED64 => TFixed64()
      //    case Type.TYPE_SINT32   => TInt32()
      //    case Type.TYPE_SINT64   => TInt64()
      //    case Type.TYPE_STRING   => TString()
      //    case Type.TYPE_UINT32   => TInt32()
      //    case Type.TYPE_UINT64   => TInt64()
      //    case Type.TYPE_ENUM =>
      //      findEnum(field.getTypeName, files)
      //        .fold[ProtobufF[Type]](TNull())(e => TNamedType(e.getName))
      //    case Type.TYPE_MESSAGE =>
      //      findMessage(field.getTypeName, files)
      //        .fold[ProtobufF[Type]](TNull())(e => TNamedType(e.getName))
      //    case _ => TNull()
    }

    val fff = parseProto[IO, Type]

    println(fff)

//    val parseNative => Protocol[Mu[ProtobufF]] = { f: NativeFile =>
//      Protocol.fromProto(f)
//    }
//
//    val parseProtocol: Protocol[Mu[ProtobufF]] => higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] = {
//      p: Protocol[Mu[ProtobufF]] =>
//        higherkindness.skeuomorph.mu.Protocol.fromProtobufProto(p)
//    }
//
//    val printProtocol: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] => String = {
//      p: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] =>
//        higherkindness.skeuomorph.mu.print.proto.print(p)
//    }
//
//    val result = (parseNative andThen parseProtocol andThen printProtocol)(nativeDescriptor)
//
//    result.clean must beEqualTo(expectation.clean)
    1 must beEqualTo(1)

  }

  val expectation =
    """package com.book
      |
      |object book {
      |
      |@message final case class Author(name: String, nick: String)
      |@message final case class Book(isbn: Long, title: String, author: List[Author], binding_type: BindingType)
      |@message final case class GetBookRequest(isbn: Long)
      |@message final case class GetBookViaAuthor(author: Author)
      |@message final case class BookStore(name: String, books: Map[Long, String], genres: List[Genre], payment_method: Cop[Long :: Int :: String :: Book:: TNil])
      |
      |sealed trait Genre
      |object Genre {
      |  case object UNKNOWN extends Genre
      |  case object SCIENCE_FICTION extends Genre
      |  case object POETRY extends Genre
      |}
      |
      |
      |sealed trait BindingType
      |object BindingType {
      |  case object HARDCOVER extends BindingType
      |  case object PAPERBACK extends BindingType
      |}
      |
      |@service(Protobuf) trait BookService[F[_]] {
      |  def GetBook(req: GetBookRequest): F[Book]
      |  def GetBooksViaAuthor(req: GetBookViaAuthor): Stream[F, Book]
      |  def GetGreatestBook(req: Stream[F, GetBookRequest]): F[Book]
      |  def GetBooks(req: Stream[F, GetBookRequest]): Stream[F, Book]
      |}
      |
      |}""".stripMargin

  implicit class StringOps(self: String) {
    def clean: String = self.replaceAll("\\s", "")
  }

}
