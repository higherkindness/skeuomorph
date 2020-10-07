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

import scala.meta._
import scala.meta.contrib._

class ProtobufProtocolSpec extends Specification with ScalaCheck {

  val workingDirectory: String   = new java.io.File(".").getCanonicalPath
  val testDirectory              = "/src/test/scala/higherkindness/skeuomorph/protobuf"
  val importRoot: Option[String] = Some(workingDirectory + testDirectory)

  val bookProtocol: Protocol[Mu[ProtobufF]] = {
    val path   = workingDirectory + s"$testDirectory/service"
    val source = ProtoSource(s"book.proto", path, importRoot)
    parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
  }

  implicit val arbCompressionType: Arbitrary[CompressionType] = Arbitrary {
    Gen.oneOf(CompressionType.Identity, CompressionType.Gzip)
  }

//  It should be possible to generate Scala code for a Mu protocol from a Proto file. $codegenProtobufProtocol
//
//  It should generate correct Scala code for a subset of the opencensus Protobuf protocol's models. $codegenOpencensus
//
//  The generated Scala code should include appropriately tagged integer types. $codegenTaggedIntegers
//
//  The generated Scala code should escape 'type' keyword in package (directory) names. $codegenGoogleApi
//
//  The generated Scala code should use the `java_package` option when `package` isn't present in the file but the `java_package` is. $codeGenProtobufOnlyJavaPackage
//
//  The generated Scala code should use the filename as a package option when neither `package` nor `java_package` are present in a file. $codegenProtobufNoPackage
//
//  The generated Scala code should handle enums when `package` is present in a file. $codeGenProtobufEnumWithPackage
//
//  The generated Scala code should handle enums when neither `package` nor `java_package` in a file. $codeGenProtobufEnumWithNoPackage

  def is = s2"""
  Protobuf Protocol

  The generated Scala code should use the `java_package` option when both `package` and `java_package` are present in a file. $codeGenProtobufJavaPackage

  """

  private def codegenProtobufProtocol =
    prop { (ct: CompressionType, useIdiom: Boolean) =>
      val toMuProtocol: Protocol[Mu[ProtobufF]] => higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] = {
        p: Protocol[Mu[ProtobufF]] => higherkindness.skeuomorph.mu.Protocol.fromProtobufProto(ct, useIdiom)(p)
      }

      val streamCtor: (Type, Type) => Type.Apply = { case (f: Type, a: Type) =>
        t"Stream[$f, $a]"
      }

      val codegen: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] => Pkg = {
        p: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] =>
          higherkindness.skeuomorph.mu.codegen.protocol(p, streamCtor).right.get
      }

      val actual = (toMuProtocol andThen codegen)(bookProtocol)

      val expected = bookExpectation(ct, useIdiom).parse[Source].get.children.head.asInstanceOf[Pkg]

      actual.isEqual(expected) :| s"""
      |Actual output:
      |$actual
      |
      |
      |Expected output:
      |$expected"
      """.stripMargin
    }

  private def bookExpectation(compressionType: CompressionType, useIdiomaticEndpoints: Boolean): String = {

    val serviceParams: String = "Protobuf" +
      (if (compressionType == CompressionType.Gzip) ", Gzip" else ", Identity") +
      (if (useIdiomaticEndpoints) ", namespace = Some(\"com.acme\"), methodNameStyle = Capitalize" else "")

    s"""package com.acme
      |
      |import _root_.higherkindness.mu.rpc.protocol._
      |
      |object book {
      |
      |final case class Book(
      |  @_root_.pbdirect.pbIndex(1) isbn: _root_.scala.Long,
      |  @_root_.pbdirect.pbIndex(2) title: _root_.java.lang.String,
      |  @_root_.pbdirect.pbIndex(3) author: _root_.scala.List[_root_.com.acme.author.Author],
      |  @_root_.pbdirect.pbIndex(9) binding_type: _root_.scala.Option[_root_.com.acme.book.BindingType],
      |  @_root_.pbdirect.pbIndex(10) rating: _root_.scala.Option[_root_.com.acme.rating.Rating],
      |  @_root_.pbdirect.pbIndex(11) `private`: _root_.scala.Boolean,
      |  @_root_.pbdirect.pbIndex(16) `type`: _root_.scala.Option[_root_.com.acme.book.`type`],
      |  @_root_.pbdirect.pbIndex(17) nearest_copy: _root_.scala.Option[_root_.com.acme.book.BookStore.Location]
      |)
      |final case class `type`(
      |  @_root_.pbdirect.pbIndex(1) foo: _root_.scala.Long,
      |  @_root_.pbdirect.pbIndex(2) thing: _root_.scala.Option[_root_.com.acme.`hyphenated-name`.Thing]
      |)
      |final case class GetBookRequest(
      |  @_root_.pbdirect.pbIndex(1) isbn: _root_.scala.Long
      |)
      |final case class GetBookViaAuthor(
      |  @_root_.pbdirect.pbIndex(1) author: _root_.scala.Option[_root_.com.acme.author.Author]
      |)
      |final case class BookStore(
      |  @_root_.pbdirect.pbIndex(1) name: _root_.java.lang.String,
      |  @_root_.pbdirect.pbIndex(2) books: _root_.scala.Predef.Map[_root_.scala.Long, _root_.java.lang.String],
      |  @_root_.pbdirect.pbIndex(3) genres: _root_.scala.List[_root_.com.acme.book.Genre],
      |  @_root_.pbdirect.pbIndex(4,5,6,7) payment_method: _root_.scala.Option[_root_.shapeless.:+:[_root_.scala.Long, _root_.shapeless.:+:[_root_.scala.Int, _root_.shapeless.:+:[_root_.java.lang.String, _root_.shapeless.:+:[_root_.com.acme.book.Book, _root_.shapeless.CNil]]]]],
      |  @_root_.pbdirect.pbIndex(8,9) either: _root_.scala.Option[_root_.scala.Either[_root_.scala.Long, _root_.scala.Int]],
      |  @_root_.pbdirect.pbIndex(10) location: _root_.scala.Option[_root_.com.acme.book.BookStore.Location],
      |  @_root_.pbdirect.pbIndex(11) coffee_quality: _root_.scala.Option[_root_.com.acme.book.BookStore.CoffeeQuality]
      |)
      |object BookStore {
      |  final case class Location(
      |    @_root_.pbdirect.pbIndex(1) town: _root_.java.lang.String,
      |    @_root_.pbdirect.pbIndex(2) country: _root_.scala.Option[_root_.com.acme.book.BookStore.Location.Country]
      |  )
      |  object Location {
      |    final case class Country(
      |      @_root_.pbdirect.pbIndex(1) name: _root_.java.lang.String,
      |      @_root_.pbdirect.pbIndex(2) iso_code: _root_.java.lang.String
      |    )
      |  }
      |  sealed abstract class CoffeeQuality(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
      |  object CoffeeQuality extends _root_.enumeratum.values.IntEnum[CoffeeQuality] {
      |    case object DELICIOUS extends CoffeeQuality(0)
      |    case object DRINKABLE extends CoffeeQuality(1)
      |
      |    val values = findValues
      |  }
      |}
      |
      |sealed abstract class Genre(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
      |object Genre extends _root_.enumeratum.values.IntEnum[Genre] {
      |  case object UNKNOWN extends Genre(0)
      |  case object SCIENCE_FICTION extends Genre(1)
      |  case object POETRY extends Genre(2)
      |
      |  val values = findValues
      |}
      |
      |sealed abstract class BindingType(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
      |object BindingType extends _root_.enumeratum.values.IntEnum[BindingType] {
      |  case object HARDCOVER extends BindingType(0)
      |  case object PAPERBACK extends BindingType(5)
      |
      |  val values = findValues
      |}
      |
      |@service($serviceParams) trait BookService[F[_]] {
      |  def GetBook(req: _root_.com.acme.book.GetBookRequest): F[_root_.com.acme.book.Book]
      |  def GetBooksViaAuthor(req: _root_.com.acme.book.GetBookViaAuthor): F[Stream[F, _root_.com.acme.book.Book]]
      |  def GetGreatestBook(req: Stream[F, _root_.com.acme.book.GetBookRequest]): F[_root_.com.acme.book.Book]
      |  def GetBooks(req: Stream[F, _root_.com.acme.book.GetBookRequest]): F[Stream[F, _root_.com.acme.book.Book]]
      |  def GetRatingOfAuthor(req: _root_.com.acme.author.Author): F[_root_.com.acme.rating.Rating]
      |}
      |
      |}""".stripMargin
  }

  private def check(protobufProtocol: Protocol[Mu[ProtobufF]], expectedOutput: String) = {
    val toMuProtocol: Protocol[Mu[ProtobufF]] => higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] = {
      p: Protocol[Mu[ProtobufF]] =>
        higherkindness.skeuomorph.mu.Protocol
          .fromProtobufProto(CompressionType.Identity, useIdiomaticEndpoints = true)(p)
    }

    val streamCtor: (Type, Type) => Type.Apply = { case (f: Type, a: Type) =>
      t"Stream[$f, $a]"
    }

    val codegen: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] => Pkg = {
      p: higherkindness.skeuomorph.mu.Protocol[Mu[MuF]] =>
        higherkindness.skeuomorph.mu.codegen.protocol(p, streamCtor).right.get
    }

    val actual = (toMuProtocol andThen codegen)(protobufProtocol)

    val expected = expectedOutput.parse[Source].get.children.head.asInstanceOf[Pkg]

    actual.isEqual(expected) :| s"""
      |Actual output:
      |$actual
      |
      |
      |Expected output:
      |$expected"
      """.stripMargin
  }

  private def codegenOpencensus = {
    val opencensusProtocol: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/models/opencensus"
      val source = ProtoSource(s"trace.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(opencensusProtocol, opencensusExpectation)
  }

  val opencensusExpectation = s"""
    |package opencensus.proto.trace.v1
    |
    |import _root_.higherkindness.mu.rpc.protocol._
    |
    |object trace {
    |  final case class Span(
    |    @_root_.pbdirect.pbIndex(1) trace_id: _root_.scala.Array[Byte],
    |    @_root_.pbdirect.pbIndex(2) span_id: _root_.scala.Array[Byte],
    |    @_root_.pbdirect.pbIndex(3) parent_span_id: _root_.scala.Array[Byte],
    |    @_root_.pbdirect.pbIndex(4) name: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString],
    |    @_root_.pbdirect.pbIndex(7) attributes: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.Attributes],
    |    @_root_.pbdirect.pbIndex(8) stack_trace: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.StackTrace],
    |    @_root_.pbdirect.pbIndex(9) time_events: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.TimeEvents],
    |    @_root_.pbdirect.pbIndex(10) links: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.Links],
    |    @_root_.pbdirect.pbIndex(11) status: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Status],
    |    @_root_.pbdirect.pbIndex(14) kind: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.SpanKind],
    |    @_root_.pbdirect.pbIndex(15) tracestate: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.Tracestate],
    |    @_root_.pbdirect.pbIndex(16) resource: _root_.scala.Option[_root_.opencensus.proto.resource.v1.resource.Resource]
    |  )
    |  object Span {
    |    final case class Tracestate(
    |      @_root_.pbdirect.pbIndex(1) entries: _root_.scala.List[_root_.opencensus.proto.trace.v1.trace.Span.Tracestate.Entry]
    |    )
    |    object Tracestate {
    |      final case class Entry(
    |        @_root_.pbdirect.pbIndex(1) key: _root_.java.lang.String,
    |        @_root_.pbdirect.pbIndex(2) value: _root_.java.lang.String
    |      )
    |    }
    |    final case class Attributes(
    |      @_root_.pbdirect.pbIndex(1) attribute_map: _root_.scala.Predef.Map[_root_.java.lang.String, _root_.opencensus.proto.trace.v1.trace.AttributeValue],
    |      @_root_.pbdirect.pbIndex(2) dropped_attributes_count: _root_.scala.Int
    |    )
    |    final case class TimeEvent(
    |      @_root_.pbdirect.pbIndex(2, 3) value: _root_.scala.Option[_root_.scala.Either[_root_.opencensus.proto.trace.v1.trace.Span.TimeEvent.Annotation, _root_.opencensus.proto.trace.v1.trace.Span.TimeEvent.MessageEvent]]
    |    )
    |    object TimeEvent {
    |      final case class Annotation(
    |        @_root_.pbdirect.pbIndex(1) description: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString],
    |        @_root_.pbdirect.pbIndex(2) attributes: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.Attributes]
    |      )
    |      final case class MessageEvent(
    |        @_root_.pbdirect.pbIndex(1) `type`: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.TimeEvent.MessageEvent.Type],
    |        @_root_.pbdirect.pbIndex(2) id: _root_.shapeless.tag.@@[_root_.scala.Long, _root_.pbdirect.Unsigned],
    |        @_root_.pbdirect.pbIndex(3) uncompressed_size: _root_.shapeless.tag.@@[_root_.scala.Long, _root_.pbdirect.Unsigned],
    |        @_root_.pbdirect.pbIndex(4) compressed_size: _root_.shapeless.tag.@@[_root_.scala.Long, _root_.pbdirect.Unsigned]
    |      )
    |      object MessageEvent {
    |        sealed abstract class Type(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
    |        object Type extends _root_.enumeratum.values.IntEnum[Type] {
    |          case object TYPE_UNSPECIFIED extends Type(0)
    |          case object SENT extends Type(1)
    |          case object RECEIVED extends Type(2)
    |          val values = findValues
    |        }
    |      }
    |    }
    |    final case class TimeEvents(
    |      @_root_.pbdirect.pbIndex(1) time_event: _root_.scala.List[_root_.opencensus.proto.trace.v1.trace.Span.TimeEvent],
    |      @_root_.pbdirect.pbIndex(2) dropped_annotations_count: _root_.scala.Int,
    |      @_root_.pbdirect.pbIndex(3) dropped_message_events_count: _root_.scala.Int
    |    )
    |    final case class Link(
    |      @_root_.pbdirect.pbIndex(1) trace_id: _root_.scala.Array[Byte],
    |      @_root_.pbdirect.pbIndex(2) span_id: _root_.scala.Array[Byte],
    |      @_root_.pbdirect.pbIndex(3) `type`: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.Link.Type],
    |      @_root_.pbdirect.pbIndex(4) attributes: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Span.Attributes]
    |    )
    |    object Link {
    |      sealed abstract class Type(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
    |      object Type extends _root_.enumeratum.values.IntEnum[Type] {
    |        case object TYPE_UNSPECIFIED extends Type(0)
    |        case object CHILD_LINKED_SPAN extends Type(1)
    |        case object PARENT_LINKED_SPAN extends Type(2)
    |        val values = findValues
    |      }
    |    }
    |    final case class Links(
    |      @_root_.pbdirect.pbIndex(1) link: _root_.scala.List[_root_.opencensus.proto.trace.v1.trace.Span.Link],
    |      @_root_.pbdirect.pbIndex(2) dropped_links_count: _root_.scala.Int
    |    )
    |    sealed abstract class SpanKind(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
    |    object SpanKind extends _root_.enumeratum.values.IntEnum[SpanKind] {
    |      case object SPAN_KIND_UNSPECIFIED extends SpanKind(0)
    |      case object SERVER extends SpanKind(1)
    |      case object CLIENT extends SpanKind(2)
    |      val values = findValues
    |    }
    |  }
    |  final case class Status(
    |    @_root_.pbdirect.pbIndex(1) code: _root_.scala.Int,
    |    @_root_.pbdirect.pbIndex(2) message: _root_.java.lang.String
    |  )
    |  final case class AttributeValue(
    |    @_root_.pbdirect.pbIndex(1, 2, 3, 4) value: _root_.scala.Option[_root_.shapeless.:+:[_root_.opencensus.proto.trace.v1.trace.TruncatableString, _root_.shapeless.:+:[_root_.scala.Long, _root_.shapeless.:+:[_root_.scala.Boolean, _root_.shapeless.:+:[_root_.scala.Double, _root_.shapeless.CNil]]]]]
    |  )
    |  final case class StackTrace(
    |    @_root_.pbdirect.pbIndex(1) stack_frames: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.StackTrace.StackFrames],
    |    @_root_.pbdirect.pbIndex(2) stack_trace_hash_id: _root_.shapeless.tag.@@[_root_.scala.Long, _root_.pbdirect.Unsigned]
    |  )
    |  object StackTrace {
    |    final case class StackFrame(
    |      @_root_.pbdirect.pbIndex(1) function_name: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString],
    |      @_root_.pbdirect.pbIndex(2) original_function_name: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString],
    |      @_root_.pbdirect.pbIndex(3) file_name: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString],
    |      @_root_.pbdirect.pbIndex(4) line_number: _root_.scala.Long,
    |      @_root_.pbdirect.pbIndex(5) column_number: _root_.scala.Long,
    |      @_root_.pbdirect.pbIndex(6) load_module: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.Module],
    |      @_root_.pbdirect.pbIndex(7) source_version: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString]
    |    )
    |    final case class StackFrames(
    |      @_root_.pbdirect.pbIndex(1) frame: _root_.scala.List[_root_.opencensus.proto.trace.v1.trace.StackTrace.StackFrame],
    |      @_root_.pbdirect.pbIndex(2) dropped_frames_count: _root_.scala.Int
    |    )
    |  }
    |  final case class Module(
    |    @_root_.pbdirect.pbIndex(1) module: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString],
    |    @_root_.pbdirect.pbIndex(2) build_id: _root_.scala.Option[_root_.opencensus.proto.trace.v1.trace.TruncatableString]
    |  )
    |  final case class TruncatableString(
    |    @_root_.pbdirect.pbIndex(1) value: _root_.java.lang.String,
    |    @_root_.pbdirect.pbIndex(2) truncated_byte_count: _root_.scala.Int
    |  )
    |}""".stripMargin

  def codegenTaggedIntegers = {
    val integerTypesProtocol: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/models"
      val source = ProtoSource(s"integer_types.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(integerTypesProtocol, taggedIntegersExpectation)
  }

  val taggedIntegersExpectation = s"""
    |package com.acme
    |
    |import _root_.higherkindness.mu.rpc.protocol._
    |
    |object integer_types {
    |  final case class IntegerTypes(
    |    @_root_.pbdirect.pbIndex(1) a: _root_.scala.Int,
    |    @_root_.pbdirect.pbIndex(2) b: _root_.shapeless.tag.@@[_root_.scala.Int, _root_.pbdirect.Unsigned],
    |    @_root_.pbdirect.pbIndex(3) c: _root_.shapeless.tag.@@[_root_.scala.Int, _root_.pbdirect.Signed],
    |    @_root_.pbdirect.pbIndex(4) d: _root_.shapeless.tag.@@[_root_.scala.Int, _root_.pbdirect.Fixed],
    |    @_root_.pbdirect.pbIndex(5) e: _root_.shapeless.tag.@@[_root_.scala.Int, (_root_.pbdirect.Fixed with _root_.pbdirect.Signed)],
    |    @_root_.pbdirect.pbIndex(6) f: _root_.scala.Long,
    |    @_root_.pbdirect.pbIndex(7) g: _root_.shapeless.tag.@@[_root_.scala.Long, _root_.pbdirect.Unsigned],
    |    @_root_.pbdirect.pbIndex(8) h: _root_.shapeless.tag.@@[_root_.scala.Long, _root_.pbdirect.Signed],
    |    @_root_.pbdirect.pbIndex(9) i: _root_.shapeless.tag.@@[_root_.scala.Long, _root_.pbdirect.Fixed],
    |    @_root_.pbdirect.pbIndex(10) j: _root_.shapeless.tag.@@[_root_.scala.Long, (_root_.pbdirect.Fixed with _root_.pbdirect.Signed)]
    |  )
    |}
    |""".stripMargin

  private def codegenGoogleApi = {
    val googleApiProtocol: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/models/type"
      val source = ProtoSource(s"date.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(googleApiProtocol, googleApiExpectation)
  }

  val googleApiExpectation = s"""
    |package com.google.`type`
    |import _root_.higherkindness.mu.rpc.protocol._
    |object date { final case class Date(@_root_.pbdirect.pbIndex(1) year: _root_.scala.Int, @_root_.pbdirect.pbIndex(2) month: _root_.scala.Int, @_root_.pbdirect.pbIndex(3) day: _root_.scala.Int) }
    |""".stripMargin

  private def codeGenProtobufOnlyJavaPackage = {
    val optionalPackage: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/packages"
      val source = ProtoSource(s"test_only_java_package.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(optionalPackage, protobufOnlyJavaPackage)
  }

  val protobufOnlyJavaPackage =
    s"""
       |package my_package
       |import _root_.higherkindness.mu.rpc.protocol._
       |object test_only_java_package {
       |  final case class MyRequest(@_root_.pbdirect.pbIndex(1) value: _root_.java.lang.String)
       |  final case class MyResponse(@_root_.pbdirect.pbIndex(1) value: _root_.java.lang.String)
       |  @service(Protobuf, Identity, namespace = Some("my_package"), methodNameStyle = Capitalize) trait MyService[F[_]] { def Check(req: _root_.my_package.test_only_java_package.MyRequest): F[_root_.my_package.test_only_java_package.MyResponse] }
       |}
       |""".stripMargin

  private def codegenProtobufNoPackage = {
    val optionalPackage: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/packages"
      val source = ProtoSource(s"test_no_package.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(optionalPackage, protobufNoJavaPackage)
  }

  val protobufNoJavaPackage =
    s"""
    |package test_no_package
    |import _root_.higherkindness.mu.rpc.protocol._
    |object test_no_package {
    |  final case class MyRequest(@_root_.pbdirect.pbIndex(1) value: _root_.java.lang.String)
    |  final case class MyResponse(@_root_.pbdirect.pbIndex(1) value: _root_.java.lang.String)
    |  @service(Protobuf, Identity, namespace = Some("test_no_package"), methodNameStyle = Capitalize) trait MyService[F[_]] { def Check(req: _root_.test_no_package.test_no_package.MyRequest): F[_root_.test_no_package.test_no_package.MyResponse] }
    |}
    |""".stripMargin

  private def codeGenProtobufJavaPackage = {
    val javaPackageAndRegularPackage: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/packages"
      val source = ProtoSource(s"test_java_package.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(javaPackageAndRegularPackage, protobufJavaPackageExpectation)
  }

  val protobufJavaPackageExpectation =
    s"""
    |package my_package
    |import _root_.higherkindness.mu.rpc.protocol._
    |object test_java_package {
    |  final case class MyRequest(@_root_.pbdirect.pbIndex(1) value: _root_.java.lang.String)
    |  final case class MyResponse(@_root_.pbdirect.pbIndex(1) value: _root_.java.lang.String)
    |  @service(Protobuf, Identity, namespace = Some("my_package"), methodNameStyle = Capitalize) trait MyService[F[_]] { def Check(req: _root_.my_package.test_java_package.MyRequest): F[_root_.my_package.test_java_package.MyResponse] }
    |}
    |""".stripMargin

  private def codeGenProtobufEnumWithPackage = {
    val enumWithPackage: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/packages"
      val source = ProtoSource(s"test_enum_package.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(enumWithPackage, protobufEnumWithPackageExpectation)
  }

  val protobufEnumWithPackageExpectation =
    s"""
    |package example
    |import _root_.higherkindness.mu.rpc.protocol._
    |object test_enum_package {
    |  final case class Example(@_root_.pbdirect.pbIndex(1) enum: _root_.scala.Option[_root_.example.test_enum_package.ExampleEnum])
    |  sealed abstract class ExampleEnum(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
    |  object ExampleEnum extends _root_.enumeratum.values.IntEnum[ExampleEnum] {
    |    case object Option1 extends ExampleEnum(0)
    |    case object Option2 extends ExampleEnum(1)
    |    val values = findValues
    |  }
    |}
    |""".stripMargin

  private def codeGenProtobufEnumWithNoPackage = {
    val enumWithNoPackage: Protocol[Mu[ProtobufF]] = {
      val path   = workingDirectory + s"$testDirectory/packages"
      val source = ProtoSource(s"test_enum_no_package.proto", path, importRoot)
      parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()
    }

    check(enumWithNoPackage, protobufEnumWithoutPackageExpectation)
  }

  val protobufEnumWithoutPackageExpectation =
    s"""
    |package test_enum_no_package
    |import _root_.higherkindness.mu.rpc.protocol._
    |object test_enum_no_package {
    |  final case class Example(@_root_.pbdirect.pbIndex(1) enum: _root_.scala.Option[_root_.test_enum_no_package.test_enum_no_package.ExampleEnum])
    |  sealed abstract class ExampleEnum(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
    |  object ExampleEnum extends _root_.enumeratum.values.IntEnum[ExampleEnum] {
    |    case object Option1 extends ExampleEnum(0)
    |    case object Option2 extends ExampleEnum(1)
    |    val values = findValues
    |  }
    |}
    |""".stripMargin
}
