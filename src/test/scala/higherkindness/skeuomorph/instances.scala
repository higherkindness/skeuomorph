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

import cats.data.NonEmptyList
import cats.implicits._
import org.apache.avro.Schema
import org.scalacheck._
import org.scalacheck.cats.implicits._
import mu.MuF
import avro.AvroF
import protobuf._
import openapi._
import qq.droste.Basis

import scala.collection.JavaConverters._

object instances {
  lazy val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

  lazy val smallNumber: Gen[Int] = Gen.choose(1, 10)

  lazy val sampleBool: Gen[Boolean] = Gen.oneOf(true, false)

  def protobufFMessageWithRepeatFields[T](withRepeat: Boolean)(
      implicit B: Basis[ProtobufF, T]): Gen[ProtobufF.TMessage[T]] = {

    val innerTypes: Gen[ProtobufF[T]] = Gen.oneOf(
      List(
        ProtobufF.TDouble[T](),
        ProtobufF.TFloat[T](),
        ProtobufF.TInt32[T](),
        ProtobufF.TInt64[T](),
        ProtobufF.TUint32[T](),
        ProtobufF.TUint64[T](),
        ProtobufF.TSint32[T](),
        ProtobufF.TSint64[T](),
        ProtobufF.TFixed32[T](),
        ProtobufF.TFixed64[T](),
        ProtobufF.TSfixed32[T](),
        ProtobufF.TSfixed64[T](),
        ProtobufF.TBool[T](),
        ProtobufF.TString[T](),
        ProtobufF.TBytes[T]()
      )
    )

    val sampleField: Gen[FieldF.Field[T]] = {
      for {
        name     <- nonEmptyString
        tpe      <- innerTypes
        position <- smallNumber
      } yield FieldF.Field(name, B.algebra(tpe), position, List(), withRepeat, isMapField = false)
    }

    for {
      name  <- nonEmptyString
      field <- sampleField
    } yield ProtobufF.TMessage(name, List(field), List())
  }

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

  implicit def muCoproductArbitrary[T](withTNull: Boolean)(implicit B: Basis[MuF, T]): Arbitrary[MuF.TCoproduct[T]] =
    Arbitrary {
      val nonNullPrimitives: Gen[MuF[T]] = Gen.oneOf(
        List(
          MuF.TString[T](),
          MuF.TBoolean[T](),
          MuF.TByteArray[T](),
          MuF.TDouble[T](),
          MuF.TFloat[T](),
          MuF.TInt[T](),
          MuF.TLong[T]()
        )
      )

      (
        nonNullPrimitives,
        if (withTNull) Gen.const(MuF.TNull[T]()) else nonNullPrimitives,
        sampleBool
      ).mapN((t1, t2, reversed) =>
        MuF.TCoproduct(if (reversed) NonEmptyList.of(B.algebra(t2), B.algebra(t1))
        else NonEmptyList.of(B.algebra(t1), B.algebra(t2))))
    }

  implicit def muArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[MuF[T]] = {

    def fieldGen: Gen[MuF.Field[T]] =
      (
        nonEmptyString,
        Gen.lzy(T.arbitrary)
      ).mapN(MuF.Field.apply)

    Arbitrary(
      Gen.oneOf(
        MuF.`null`[T]().pure[Gen],
        MuF.double[T]().pure[Gen],
        MuF.float[T]().pure[Gen],
        MuF.int[T]().pure[Gen],
        MuF.long[T]().pure[Gen],
        MuF.boolean[T]().pure[Gen],
        MuF.string[T]().pure[Gen],
        MuF.byteArray[T]().pure[Gen],
        nonEmptyString map MuF.namedType[T],
        T.arbitrary map MuF.option[T],
        (T.arbitrary, T.arbitrary) mapN { (a, b) =>
          MuF.either(a, b)
        },
        T.arbitrary map MuF.list[T],
        T.arbitrary map (t => MuF.map[T](None, t)),
        T.arbitrary map MuF.required[T],
        (T.arbitrary, Gen.listOf(T.arbitrary)) mapN { (a, b) =>
          MuF.generic[T](a, b)
        },
        (T.arbitrary, Gen.listOf(T.arbitrary)) mapN { (a, b) =>
          MuF.generic[T](a, b)
        },
        Gen.nonEmptyListOf(T.arbitrary) map { l =>
          MuF.coproduct[T](NonEmptyList.fromListUnsafe(l))
        },
        (nonEmptyString, Gen.nonEmptyListOf(Gen.lzy(fieldGen))).mapN { (n, f) =>
          MuF.product(n, f)
        }
      ))
  }

  implicit def jsonSchemaOpenApiArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[JsonSchemaF[T]] = {
    val propertyGen: Gen[JsonSchemaF.Property[T]] = (nonEmptyString, T.arbitrary).mapN(JsonSchemaF.Property[T])
    val objectGen: Gen[JsonSchemaF[T]] = (
      Gen.listOf(propertyGen),
      Gen.listOf(nonEmptyString)
    ).mapN(JsonSchemaF.`object`[T])

    Arbitrary(
      Gen.oneOf(
        JsonSchemaF.integer[T]().pure[Gen],
        JsonSchemaF.long[T]().pure[Gen],
        JsonSchemaF.float[T]().pure[Gen],
        JsonSchemaF.double[T]().pure[Gen],
        JsonSchemaF.string[T]().pure[Gen],
        JsonSchemaF.byte[T]().pure[Gen],
        JsonSchemaF.binary[T]().pure[Gen],
        JsonSchemaF.boolean[T]().pure[Gen],
        JsonSchemaF.date[T].pure[Gen],
        JsonSchemaF.dateTime[T].pure[Gen],
        JsonSchemaF.password[T]().pure[Gen],
        T.arbitrary map JsonSchemaF.array,
        Gen.listOf(nonEmptyString) map JsonSchemaF.enum[T],
        objectGen,
        nonEmptyString.map(JsonSchemaF.reference[T])
      )
    )
  }

  implicit def avroArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[AvroF[T]] = {

    val orderGen: Gen[AvroF.Order] = Gen.oneOf(AvroF.Order.Ascending, AvroF.Order.Descending, AvroF.Order.Ignore)

    val fieldGen: Gen[AvroF.Field[T]] = (
      nonEmptyString,
      Gen.listOf(nonEmptyString),
      Gen.option(nonEmptyString),
      Gen.option(orderGen),
      T.arbitrary
    ).mapN(AvroF.Field.apply[T])

    Arbitrary(
      Gen.oneOf(
        AvroF.`null`[T]().pure[Gen],
        AvroF.boolean[T]().pure[Gen],
        AvroF.int[T]().pure[Gen],
        AvroF.long[T]().pure[Gen],
        AvroF.float[T]().pure[Gen],
        AvroF.double[T]().pure[Gen],
        AvroF.bytes[T]().pure[Gen],
        AvroF.string[T]().pure[Gen],
        nonEmptyString map AvroF.namedType[T],
        T.arbitrary map AvroF.array[T],
        T.arbitrary map AvroF.map[T],
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.option(nonEmptyString),
          Gen.listOf(fieldGen)
        ).mapN(AvroF.record[T]),
        Gen.nonEmptyListOf(T.arbitrary) map { l =>
          AvroF.union[T](NonEmptyList.fromListUnsafe(l))
        },
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString)
        ).mapN(AvroF.enum[T]),
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.posNum[Int]
        ).mapN(AvroF.fixed[T])
      ))
  }

  implicit def protoArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[ProtobufF[T]] = {
    val genOption: Gen[ProtobufF.OptionValue] = (nonEmptyString, nonEmptyString).mapN(ProtobufF.OptionValue.apply)
    Arbitrary(
      Gen.oneOf(
        ProtobufF.double[T]().pure[Gen],
        ProtobufF.float[T]().pure[Gen],
        ProtobufF.int32[T]().pure[Gen],
        ProtobufF.int64[T]().pure[Gen],
        ProtobufF.uint32[T]().pure[Gen],
        ProtobufF.uint64[T]().pure[Gen],
        ProtobufF.sint32[T]().pure[Gen],
        ProtobufF.sint64[T]().pure[Gen],
        ProtobufF.fixed32[T]().pure[Gen],
        ProtobufF.fixed64[T]().pure[Gen],
        ProtobufF.sfixed32[T]().pure[Gen],
        ProtobufF.sfixed64[T]().pure[Gen],
        ProtobufF.bool[T]().pure[Gen],
        ProtobufF.string[T]().pure[Gen],
        ProtobufF.bytes[T]().pure[Gen],
        nonEmptyString map ProtobufF.namedType[T],
        T.arbitrary map ProtobufF.repeated[T],
        (
          nonEmptyString,
          Gen.listOf((nonEmptyString, Gen.posNum[Int]).tupled),
          Gen.listOf(genOption),
          Gen.listOf((nonEmptyString, Gen.posNum[Int]).tupled)
        ).mapN(ProtobufF.enum[T]),
        (
          nonEmptyString,
          Gen.listOf(
            (
              nonEmptyString,
              T.arbitrary,
              Gen.posNum[Int],
              Gen.listOf(genOption),
              sampleBool,
              sampleBool
            ).mapN(FieldF.Field.apply[T])
          ),
          Gen.listOf(Gen.listOf(nonEmptyString))
        ).mapN(ProtobufF.message[T])
      )
    )
  }

  def muCoproductWithTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = true).arbitrary

  def muCoproductWithoutTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = false).arbitrary
}
