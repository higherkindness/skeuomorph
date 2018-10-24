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

package skeuomorph
package avro

import cats.data.NonEmptyList
import cats.implicits._
import org.apache.avro.Schema
import org.apache.avro.Schema.Type
import org.scalacheck._
import org.scalacheck.cats.implicits._
import qq.droste.Basis
import skeuomorph.freestyle.FreesF

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

  implicit def freestyleCoproductArbitrary[T](withTNull: Boolean)(
      implicit B: Basis[FreesF, T]): Arbitrary[FreesF.TCoproduct[T]] =
    Arbitrary {
      val nonNullPrimitives: Gen[FreesF[T]] = Gen.oneOf(
        List(
          FreesF.TString[T](),
          FreesF.TBoolean[T](),
          FreesF.TByteArray[T](),
          FreesF.TDouble[T](),
          FreesF.TFloat[T](),
          FreesF.TInt[T](),
          FreesF.TLong[T]()
        )
      )

      (
        nonNullPrimitives,
        if (withTNull) Gen.const(FreesF.TNull[T]()) else nonNullPrimitives,
        Gen.oneOf(true, false)
      ).mapN((t1, t2, reversed) =>
        FreesF.TCoproduct(if (reversed) NonEmptyList.of(B.algebra(t2), B.algebra(t1))
        else NonEmptyList.of(B.algebra(t1), B.algebra(t2))))
    }

  def freestyleCoproductWithTNullGen[T](implicit B: Basis[FreesF, T]): Gen[FreesF.TCoproduct[T]] =
    freestyleCoproductArbitrary(withTNull = true).arbitrary

  def freestyleCoproductWithoutTNullGen[T](implicit B: Basis[FreesF, T]): Gen[FreesF.TCoproduct[T]] =
    freestyleCoproductArbitrary(withTNull = false).arbitrary
}
