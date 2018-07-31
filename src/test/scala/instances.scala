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

import scala.collection.JavaConverters._
import org.apache.avro.{Schema => AvroSchema}
import org.apache.avro.Schema.{Type => AvroType}

import cats.implicits._
import org.scalacheck._
import org.scalacheck.cats.implicits._

object instances {

  implicit val avroSchemaArbitrary: Arbitrary[AvroSchema] = Arbitrary {
    def createGen(tpe: AvroType): Gen[AvroSchema] =
      Gen.const(AvroSchema.create(tpe))

    val primitives: Gen[AvroSchema] = Gen.oneOf(
      createGen(AvroType.STRING),
      createGen(AvroType.BOOLEAN),
      createGen(AvroType.BYTES),
      createGen(AvroType.DOUBLE),
      createGen(AvroType.FLOAT),
      createGen(AvroType.INT),
      createGen(AvroType.LONG),
      createGen(AvroType.NULL)
    )

    val nonEmptyString: Gen[String] = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)

    val arrayOrMap: Gen[AvroSchema] =
      Gen.oneOf(primitives.map(AvroSchema.createMap), primitives.map(AvroSchema.createArray))

    val union: Gen[AvroSchema] =
      Gen.nonEmptyContainerOf[Set, AvroSchema](primitives).map(l => AvroSchema.createUnion(l.toList.asJava))

    def field(name: String): Gen[AvroSchema.Field] =
      for {
        schema <- Gen.oneOf(primitives, arrayOrMap, union)
        doc    <- nonEmptyString
      } yield new AvroSchema.Field(name, schema, doc, null.asInstanceOf[Any])

    val record: Gen[AvroSchema] = (
      nonEmptyString,
      nonEmptyString,
      nonEmptyString,
      Gen.nonEmptyContainerOf[Set, String](nonEmptyString).map(_.toList) flatMap { l: List[String] =>
        l.traverse(field)
      }
    ).mapN {
      case (name, doc, namespace, fields) =>
        AvroSchema.createRecord(name, doc, namespace, false, fields.asJava)
    }

    Gen.oneOf(primitives, arrayOrMap, union, record)
  }

}
