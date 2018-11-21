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

package higherkindness.skeuomorph

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
  val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

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
        nonEmptyString map { str =>
          MuF.namedType[T](str)
        },
        T.arbitrary map { t =>
          MuF.option[T](t)
        },
        (T.arbitrary, T.arbitrary) mapN { (a, b) =>
          MuF.either(a, b)
        },
        T.arbitrary map { t =>
          MuF.list[T](t)
        },
        T.arbitrary map { t =>
          MuF.map[T](t)
        },
        T.arbitrary map { t =>
          MuF.required[T](t)
        },
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

  def muCoproductWithTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = true).arbitrary

  def muCoproductWithoutTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = false).arbitrary
}
