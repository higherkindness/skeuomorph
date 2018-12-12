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

package higherkindness.skeuomorph.avro

import cats.data.NonEmptyList
import cats.implicits._
import higherkindness.skeuomorph.mu.MuF
import org.apache.avro.Schema
import org.apache.avro.Schema.Type
import org.scalacheck._
import org.scalacheck.cats.implicits._
import qq.droste.Basis

import scala.collection.JavaConverters._

object instances {

  implicit val avroSchemaArbitrary: Arbitrary[Schema] = Arbitrary {
    val primitives: Gen[Schema] = Gen.oneOf(
      List(
        Type.STRING,
        Type.BOOLEAN,
        Type.BYTES,
        Type.DOUBLE,
        Type.FLOAT,
        Type.INT,
        Type.LONG,
        Type.NULL
      ).map(Schema.create)
    )

    val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

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
        Gen.oneOf(true, false)
      ).mapN((t1, t2, reversed) =>
        MuF.TCoproduct(if (reversed) NonEmptyList.of(B.algebra(t2), B.algebra(t1))
        else NonEmptyList.of(B.algebra(t1), B.algebra(t2))))
    }

  def muCoproductWithTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = true).arbitrary

  def muCoproductWithoutTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = false).arbitrary
}
