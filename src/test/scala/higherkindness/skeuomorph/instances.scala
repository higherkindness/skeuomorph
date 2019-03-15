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

import cats.implicits._
import org.apache.avro.Schema
import org.scalacheck._
import org.scalacheck.cats.implicits._

import higherkindness.skeuomorph.uast.ArbitraryKMaterializer
// import higherkindness.skeuomorph.uast.types._
// import higherkindness.skeuomorph.uast.derivation._
import higherkindness.skeuomorph.uast.arbitraries._
//import higherkindness.skeuomorph.protobuf._
import higherkindness.skeuomorph.avro.types._
import higherkindness.skeuomorph.protobuf.OptionValue
import qq.droste.Delay

import scala.collection.JavaConverters._

import iota.{CopK, TListK}

object instances {

  implicit def copkArbitrary[LL <: TListK](implicit M: ArbitraryKMaterializer[LL]): Delay[Arbitrary, CopK[LL, ?]] =
    M.materialize(offset = 0)

  implicit def arbAvroMetadata: Arbitrary[AvroMetadata] = {
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

  val nonEmptyString = Gen.nonEmptyListOf(Gen.oneOf(Gen.alphaNumChar, Gen.alphaChar, Gen.const(' '))).map(_.mkString)

  implicit def arbOptionValue: Arbitrary[OptionValue] =
    Arbitrary((nonEmptyString, nonEmptyString).mapN(OptionValue.apply))
  implicit val arb32       = Arbitrary(Gen.const(protobuf.annotations.`32`()))
  implicit val arb64       = Arbitrary(Gen.const(protobuf.annotations.`64`()))
  implicit val arbSigned   = Arbitrary(Gen.const(protobuf.annotations.Signed()))
  implicit val arbUnsigned = Arbitrary(Gen.const(protobuf.annotations.Unsigned()))
  implicit val arbFixed    = Arbitrary(Gen.const(protobuf.annotations.Fixed()))
  implicit val arbReserved = Arbitrary(Gen.listOf(Gen.listOf(nonEmptyString)).map(protobuf.annotations.Reserved.apply))

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

  implicit def muArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[mu.Type[T]] =
    implicitly[Delay[Arbitrary, mu.Type]].apply(T)

  implicit def avroArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[avro.Type[T]] =
    implicitly[Delay[Arbitrary, avro.Type]].apply(T)

  implicit def protoArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[protobuf.Type[T]] =
    implicitly[Delay[Arbitrary, protobuf.Type]].apply(T)
}
