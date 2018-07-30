/*
 * Copyright 2017-2018 47 Degrees, LLC. <http://www.47deg.com>
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

import org.apache.avro.{Schema => AvroSchema, Protocol => AvroProtocol}
import org.apache.avro.Schema.{Type => AvroType}

import scala.collection.JavaConverters._

import cats.data.NonEmptyList
import turtles._
import turtles.implicits._

object util {

  import Schema._

  def order2Order(avroO: AvroSchema.Field.Order): Order = avroO match {
    case AvroSchema.Field.Order.ASCENDING  => Order.Ascending
    case AvroSchema.Field.Order.DESCENDING => Order.Descending
    case AvroSchema.Field.Order.IGNORE     => Order.Ignore
  }

  def field2Field(avroF: AvroSchema.Field): Field[AvroSchema] = Field(
    avroF.name,
    avroF.aliases.asScala.toList,
    Option(avroF.doc),
    Option(order2Order(avroF.order)),
    avroF.schema
  )

  def fromProto[T](proto: AvroProtocol)(implicit C: Corecursive.Aux[T, Schema]): Protocol[T] = {
    Protocol(
      proto.getName,
      Option(proto.getNamespace),
      proto.getTypes.asScala.toList.map(_.ana[T](fromAvro)),
      proto.getMessages.asScala
        .map({
          case (_, message) =>
            Message[T](message.getName, message.getRequest.ana[T](fromAvro), message.getResponse.ana[T](fromAvro))
        })
        .toList
    )
  }

  /**
   * Convert [[org.apache.avro.Schema]] to [[skeuomorph.avro.AvroSchema]]
   */
  def fromAvro: Coalgebra[Schema, AvroSchema] = { sch =>
    sch.getType match {
      case AvroType.STRING  => Schema.TString()
      case AvroType.BOOLEAN => Schema.TBoolean()
      case AvroType.BYTES   => Schema.TBytes()
      case AvroType.DOUBLE  => Schema.TDouble()
      case AvroType.FLOAT   => Schema.TFloat()
      case AvroType.INT     => Schema.TInt()
      case AvroType.LONG    => Schema.TLong()
      case AvroType.NULL    => Schema.TNull()
      case AvroType.MAP     => Schema.TMap(sch.getValueType)
      case AvroType.ARRAY   => Schema.TArray(sch.getElementType)
      case AvroType.RECORD =>
        Schema.TRecord(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          Option(sch.getDoc),
          sch.getFields.asScala.toList.map(field2Field)
        )
      case AvroType.ENUM =>
        val symbols = sch.getEnumSymbols.asScala.toList
        Schema.TEnum(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          Option(sch.getDoc),
          symbols
        )
      case AvroType.UNION =>
        val types = sch.getTypes.asScala.toList
        Schema.TUnion(
          NonEmptyList.fromListUnsafe(types)
        )
      case AvroType.FIXED =>
        Schema.TFixed(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          sch.getFixedSize
        )
    }
  }
}
