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
import qq.droste.Coalgebra
import org.apache.avro.Schema
import org.apache.avro.Schema.Type

sealed trait AvroF[A]
object AvroF {

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

  case class TNull[A]()                    extends AvroF[A]
  case class TBoolean[A]()                 extends AvroF[A]
  case class TInt[A]()                     extends AvroF[A]
  case class TLong[A]()                    extends AvroF[A]
  case class TFloat[A]()                   extends AvroF[A]
  case class TDouble[A]()                  extends AvroF[A]
  case class TBytes[A]()                   extends AvroF[A]
  case class TString[A]()                  extends AvroF[A]
  case class TNamedType[A](name: TypeName) extends AvroF[A]
  case class TArray[A](item: A)            extends AvroF[A]
  case class TMap[A](values: A)            extends AvroF[A]
  case class TRecord[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      fields: List[Field[A]])
      extends AvroF[A]
  case class TEnum[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      symbols: List[String])
      extends AvroF[A]
  case class TUnion[A](options: NonEmptyList[A])                                                      extends AvroF[A]
  case class TFixed[A](name: TypeName, namespace: Option[String], aliases: List[TypeName], size: Int) extends AvroF[A]

  implicit val avroFunctor: Functor[AvroF] = new Functor[AvroF] {
    def map[A, B](fa: AvroF[A])(f: A => B): AvroF[B] = fa match {
      case AvroF.TNull()          => AvroF.TNull()
      case AvroF.TBoolean()       => AvroF.TBoolean()
      case AvroF.TInt()           => AvroF.TInt()
      case AvroF.TLong()          => AvroF.TLong()
      case AvroF.TFloat()         => AvroF.TFloat()
      case AvroF.TDouble()        => AvroF.TDouble()
      case AvroF.TBytes()         => AvroF.TBytes()
      case AvroF.TString()        => AvroF.TString()
      case AvroF.TNamedType(name) => AvroF.TNamedType(name)
      case AvroF.TArray(item)     => AvroF.TArray(f(item))
      case AvroF.TMap(values)     => AvroF.TMap(f(values))
      case AvroF.TRecord(name, namespace, aliases, doc, fields) =>
        AvroF.TRecord(name, namespace, aliases, doc, fields.map(field => field.copy(tpe = f(field.tpe))))
      case AvroF.TEnum(name, namespace, aliases, doc, symbols) =>
        AvroF.TEnum(name, namespace, aliases, doc, symbols)
      case AvroF.TUnion(options)                        => AvroF.TUnion(options.map(f))
      case AvroF.TFixed(name, namespace, aliases, size) => AvroF.TFixed(name, namespace, aliases, size)
    }
  }

  /**
   * Convert [[org.apache.avro.Schema]] to [[skeuomorph.avro.Schema]]
   */
  def fromAvro: Coalgebra[AvroF, Schema] = Coalgebra { sch =>
    sch.getType match {
      case Type.STRING  => AvroF.TString()
      case Type.BOOLEAN => AvroF.TBoolean()
      case Type.BYTES   => AvroF.TBytes()
      case Type.DOUBLE  => AvroF.TDouble()
      case Type.FLOAT   => AvroF.TFloat()
      case Type.INT     => AvroF.TInt()
      case Type.LONG    => AvroF.TLong()
      case Type.NULL    => AvroF.TNull()
      case Type.MAP     => AvroF.TMap(sch.getValueType)
      case Type.ARRAY   => AvroF.TArray(sch.getElementType)
      case Type.RECORD =>
        AvroF.TRecord(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          Option(sch.getDoc),
          sch.getFields.asScala.toList.map(field2Field)
        )
      case Type.ENUM =>
        val symbols = sch.getEnumSymbols.asScala.toList
        AvroF.TEnum(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          Option(sch.getDoc),
          symbols
        )
      case Type.UNION =>
        val types = sch.getTypes.asScala.toList
        AvroF.TUnion(
          NonEmptyList.fromListUnsafe(types)
        )
      case Type.FIXED =>
        AvroF.TFixed(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          sch.getFixedSize
        )
    }
  }

  def order2Order(avroO: Schema.Field.Order): Order = avroO match {
    case Schema.Field.Order.ASCENDING  => Order.Ascending
    case Schema.Field.Order.DESCENDING => Order.Descending
    case Schema.Field.Order.IGNORE     => Order.Ignore
  }

  def field2Field(avroF: Schema.Field): Field[Schema] = Field(
    avroF.name,
    avroF.aliases.asScala.toList,
    Option(avroF.doc),
    Option(order2Order(avroF.order)),
    avroF.schema
  )

}
