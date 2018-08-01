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

import cats.Functor
import cats.data.NonEmptyList
import org.apache.avro.{Schema => AvroSchema}
import org.apache.avro.Schema.{Type => AvroType}
import qq.droste.Coalgebra

sealed trait Schema[A]
object Schema {

  sealed trait Order
  object Order {
    case object Ascending  extends Order
    case object Descending extends Order
    case object Ignore     extends Order
  }

  case class Field[A](
      name: String,
      aliases: List[String],
      doc: Option[String],
      order: Option[Order],
      tpe: A
  )

  type TypeName = String

  case class TNull[A]()                    extends Schema[A]
  case class TBoolean[A]()                 extends Schema[A]
  case class TInt[A]()                     extends Schema[A]
  case class TLong[A]()                    extends Schema[A]
  case class TFloat[A]()                   extends Schema[A]
  case class TDouble[A]()                  extends Schema[A]
  case class TBytes[A]()                   extends Schema[A]
  case class TString[A]()                  extends Schema[A]
  case class TNamedType[A](name: TypeName) extends Schema[A]
  case class TArray[A](item: A)            extends Schema[A]
  case class TMap[A](values: A)            extends Schema[A]
  case class TRecord[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      fields: List[Field[A]])
      extends Schema[A]
  case class TEnum[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      symbols: List[String])
      extends Schema[A]
  case class TUnion[A](options: NonEmptyList[A])                                                      extends Schema[A]
  case class TFixed[A](name: TypeName, namespace: Option[String], aliases: List[TypeName], size: Int) extends Schema[A]

  implicit val schemaFunctor: Functor[Schema] = new Functor[Schema] {
    def map[A, B](fa: Schema[A])(f: A => B): Schema[B] = fa match {
      case Schema.TNull()          => Schema.TNull()
      case Schema.TBoolean()       => Schema.TBoolean()
      case Schema.TInt()           => Schema.TInt()
      case Schema.TLong()          => Schema.TLong()
      case Schema.TFloat()         => Schema.TFloat()
      case Schema.TDouble()        => Schema.TDouble()
      case Schema.TBytes()         => Schema.TBytes()
      case Schema.TString()        => Schema.TString()
      case Schema.TNamedType(name) => Schema.TNamedType(name)
      case Schema.TArray(item)     => Schema.TArray(f(item))
      case Schema.TMap(values)     => Schema.TMap(f(values))
      case Schema.TRecord(name, namespace, aliases, doc, fields) =>
        Schema.TRecord(name, namespace, aliases, doc, fields.map(field => field.copy(tpe = f(field.tpe))))
      case Schema.TEnum(name, namespace, aliases, doc, symbols) =>
        Schema.TEnum(name, namespace, aliases, doc, symbols)
      case Schema.TUnion(options)                        => Schema.TUnion(options.map(f))
      case Schema.TFixed(name, namespace, aliases, size) => Schema.TFixed(name, namespace, aliases, size)
    }
  }

  /**
   * Convert [[org.apache.avro.Schema]] to [[skeuomorph.avro.AvroSchema]]
   */
  def fromAvro: Coalgebra[Schema, AvroSchema] = Coalgebra { sch =>
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

}
