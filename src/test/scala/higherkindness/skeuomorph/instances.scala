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

package higherkindness.skeuomorph

import cats.Eq
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.traverse._

import org.apache.avro.Schema
import org.scalacheck._
import org.scalacheck.cats.implicits._

import higherkindness.skeuomorph.uast.{ArbitraryKMaterializer, Delay}
import higherkindness.skeuomorph.uast.arbitraries._
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.avro._
import higherkindness.skeuomorph.avro.types._
import higherkindness.skeuomorph.protobuf._

import scala.collection.JavaConverters._

import iota.{CopK, TListK}

object instances {

  val nonEmptyString = Gen.nonEmptyListOf(Gen.oneOf(Gen.alphaNumChar, Gen.alphaChar, Gen.const(' '))).map(_.mkString)

  implicit def copkArbitrary[LL <: TListK](implicit M: ArbitraryKMaterializer[LL]): Delay[Arbitrary, CopK[LL, ?]] =
    M.materialize(offset = 0)

  implicit val arbAvroMetadata: Arbitrary[AvroMetadata] = {
    val genOrder  = Gen.oneOf(Order.Ascending, Order.Descending, Order.Ignore)
    val aliases   = Gen.listOf(nonEmptyString).map(AvroMetadata.Aliases)
    val namespace = Gen.option(nonEmptyString).map(AvroMetadata.NameSpace)
    val doc       = Gen.option(nonEmptyString).map(AvroMetadata.Doc)
    val order     = Gen.option(genOrder).map(AvroMetadata.AMOrder)
    val list      = Gen.listOf(Gen.oneOf(aliases, namespace, doc, order)).map(AvroMetadata.AMList)

    Arbitrary(
      Gen.lzy(
        Gen.oneOf(
          aliases,
          namespace,
          doc,
          order,
          list
        )
      ))
  }

  implicit val arbOptionValue: Arbitrary[(String, String)] =
    Arbitrary((nonEmptyString, nonEmptyString).tupled)
  implicit val arb32: Arbitrary[protobuf.annotations.`32`]       = Arbitrary(Gen.const(protobuf.annotations.`32`()))
  implicit val arb64: Arbitrary[protobuf.annotations.`64`]       = Arbitrary(Gen.const(protobuf.annotations.`64`()))
  implicit val arbSigned: Arbitrary[protobuf.annotations.Signed] = Arbitrary(Gen.const(protobuf.annotations.Signed()))
  implicit val arbUnsigned: Arbitrary[protobuf.annotations.Unsigned] = Arbitrary(
    Gen.const(protobuf.annotations.Unsigned()))
  implicit val arbFixed: Arbitrary[protobuf.annotations.Fixed] = Arbitrary(Gen.const(protobuf.annotations.Fixed()))
  implicit val arbReserved: Arbitrary[protobuf.annotations.Reserved] = Arbitrary(
    Gen.listOf(Gen.listOf(nonEmptyString)).map(protobuf.annotations.Reserved.apply))

  implicit val arbTAvroRecord: Delay[Arbitrary, TAvroRecord] = arbitraryAnnotated[TRecord, AvroMetadata]
  implicit val arbTInt32: Delay[Arbitrary, TInt32]           = arbitraryAnnotated[TInt, annotations.`32`]
  implicit val arbTInt64: Delay[Arbitrary, TInt64]           = arbitraryAnnotated[TInt, annotations.`64`]
  implicit val arbTUInt32: Delay[Arbitrary, TUInt32] =
    arbitraryAnnotated[TInt, (annotations.Unsigned, annotations.`32`)]
  implicit val arbTUInt64: Delay[Arbitrary, TUInt64] =
    arbitraryAnnotated[TInt, (annotations.Unsigned, annotations.`64`)]
  implicit val arbTSInt32: Delay[Arbitrary, TSInt32]   = arbitraryAnnotated[TInt, (annotations.Signed, annotations.`32`)]
  implicit val arbTSInt64: Delay[Arbitrary, TSInt64]   = arbitraryAnnotated[TInt, (annotations.Signed, annotations.`64`)]
  implicit val arbTFixed32: Delay[Arbitrary, TFixed32] = arbitraryAnnotated[TInt, (annotations.Fixed, annotations.`32`)]
  implicit val arbTFixed64: Delay[Arbitrary, TFixed64] = arbitraryAnnotated[TInt, (annotations.Fixed, annotations.`64`)]
  implicit val arbTSFixed32: Delay[Arbitrary, TSFixed32] =
    arbitraryAnnotated[TInt, (annotations.Fixed, annotations.Signed, annotations.`32`)]
  implicit val arbTSFixed64: Delay[Arbitrary, TSFixed64] =
    arbitraryAnnotated[TInt, (annotations.Fixed, annotations.Signed, annotations.`64`)]
  implicit val arbTMessage: Delay[Arbitrary, TMessage] = arbitraryAnnotated[TRecord, annotations.Reserved]

  implicit val avroSchemaArbitrary: Arbitrary[Schema] = Arbitrary {
    val primitives: Gen[Schema] = Gen.oneOf(
      List(
        org.apache.avro.Schema.Type.STRING,
        org.apache.avro.Schema.Type.BOOLEAN,
        org.apache.avro.Schema.Type.BYTES,
        org.apache.avro.Schema.Type.DOUBLE,
        org.apache.avro.Schema.Type.FLOAT,
        org.apache.avro.Schema.Type.INT,
        org.apache.avro.Schema.Type.LONG,
        org.apache.avro.Schema.Type.NULL
      ).map(Schema.create)
    )

    val arrayOrMap: Gen[Schema] =
      Gen.oneOf(primitives.map(Schema.createMap), primitives.map(Schema.createArray))

    val union: Gen[Schema] =
      Gen.nonEmptyContainerOf[Set, Schema](primitives).map(l => Schema.createUnion(l.toList.asJava))

    def field(name: String): Gen[Schema.Field] =
      for {
        schema <- Gen.oneOf(primitives, arrayOrMap, union)
        doc    <- nonEmptyString
      } yield new Schema.Field(name, schema, doc, null.asInstanceOf[Any])

    val record: Gen[Schema] = (
      nonEmptyString,
      nonEmptyString,
      nonEmptyString,
      Gen.nonEmptyContainerOf[Set, String](nonEmptyString).map(_.toList) flatMap { l: List[String] =>
        l.traverse(field)
      }
    ).mapN {
      case (name, doc, namespace, fields) =>
        Schema.createRecord(name, doc, namespace, false, fields.asJava)
    }

    Gen.oneOf(primitives, arrayOrMap, union, record)
  }

  implicit val muArbitrary: Delay[Arbitrary, mu.Type]           = copkArbitrary[mu.Types]
  implicit val avroArbitrary: Delay[Arbitrary, avro.Type]       = copkArbitrary[avro.Types]
  implicit val protoArbitrary: Delay[Arbitrary, protobuf.Type]  = copkArbitrary[protobuf.Types]
  implicit val openapiArbitrary: Delay[Arbitrary, openapi.Type] = copkArbitrary[openapi.Types]

  implicit def delayArbitrary[F[_], A](
      implicit
      F: Delay[Arbitrary, F],
      A: Arbitrary[A]): Arbitrary[F[A]] = F(A)

  implicit def delayEq[F[_], A](
      implicit
      F: Delay[Eq, F],
      A: Eq[A]): Eq[F[A]] = F(A)
}
