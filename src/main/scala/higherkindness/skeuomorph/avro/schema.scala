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

import cats.Functor
import cats.data.NonEmptyList
import io.circe.Json
import org.apache.avro.Schema
import org.apache.avro.Schema.Type
import qq.droste.{Algebra, Coalgebra}

import scala.collection.JavaConverters._

sealed trait AvroF[A]
object AvroF {

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

  def field2Obj(f: Field[Json]): Json =
    Json.obj(
      "name"    -> Json.fromString(f.name),
      "aliases" -> Json.arr(f.aliases.map(Json.fromString): _*),
      "type"    -> f.tpe
    )

  sealed trait Order
  object Order {
    case object Ascending  extends Order
    case object Descending extends Order
    case object Ignore     extends Order
  }

  final case class Field[A](
      name: String,
      aliases: List[String],
      doc: Option[String],
      order: Option[Order],
      tpe: A
  )

  type TypeName = String

  final case class TNull[A]()                    extends AvroF[A]
  final case class TBoolean[A]()                 extends AvroF[A]
  final case class TInt[A]()                     extends AvroF[A]
  final case class TLong[A]()                    extends AvroF[A]
  final case class TFloat[A]()                   extends AvroF[A]
  final case class TDouble[A]()                  extends AvroF[A]
  final case class TBytes[A]()                   extends AvroF[A]
  final case class TString[A]()                  extends AvroF[A]
  final case class TNamedType[A](name: TypeName) extends AvroF[A]
  final case class TArray[A](item: A)            extends AvroF[A]
  final case class TMap[A](values: A)            extends AvroF[A]
  final case class TRecord[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      fields: List[Field[A]])
      extends AvroF[A]
  final case class TEnum[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      symbols: List[String])
      extends AvroF[A]
  final case class TUnion[A](options: NonEmptyList[A]) extends AvroF[A]
  final case class TFixed[A](name: TypeName, namespace: Option[String], aliases: List[TypeName], size: Int)
      extends AvroF[A]

  /**
   * Helper methods to construct AvroF values.  These methods are to
   * avoid scala infering the case type instead of AvroF.
   */
  def tNull[A](): AvroF[A]                    = TNull[A]()
  def tBoolean[A](): AvroF[A]                 = TBoolean[A]()
  def tInt[A](): AvroF[A]                     = TInt[A]()
  def tLong[A](): AvroF[A]                    = TLong[A]()
  def tFloat[A](): AvroF[A]                   = TFloat[A]()
  def tDouble[A](): AvroF[A]                  = TDouble[A]()
  def tBytes[A](): AvroF[A]                   = TBytes[A]()
  def tString[A](): AvroF[A]                  = TString[A]()
  def tNamedType[A](name: TypeName): AvroF[A] = TNamedType[A](name)
  def tArray[A](item: A): AvroF[A]            = TArray[A](item)
  def tMap[A](values: A): AvroF[A]            = TMap[A](values)
  def tRecord[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      fields: List[Field[A]]): AvroF[A] = TRecord(name, namespace, aliases, doc, fields)
  def tEnum[A](
      name: TypeName,
      namespace: Option[String],
      aliases: List[TypeName],
      doc: Option[String],
      symbols: List[String]): AvroF[A]              = TEnum(name, namespace, aliases, doc, symbols)
  def tUnion[A](options: NonEmptyList[A]): AvroF[A] = TUnion(options)
  def tFixed[A](name: TypeName, namespace: Option[String], aliases: List[TypeName], size: Int): AvroF[A] =
    TFixed(name, namespace, aliases, size)

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
   * Convert org.apache.avro.Schema to skeuomorph.avro.Schema
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

  def toJson: Algebra[AvroF, Json] = Algebra {
    case TNull()          => Json.fromString("Null")
    case TBoolean()       => Json.fromString("Boolean")
    case TInt()           => Json.fromString("Int")
    case TLong()          => Json.fromString("Long")
    case TFloat()         => Json.fromString("Float")
    case TDouble()        => Json.fromString("Double")
    case TBytes()         => Json.fromString("Bytes")
    case TString()        => Json.fromString("String")
    case TNamedType(name) => Json.fromString(name)
    case TArray(item) =>
      Json.obj(
        "type"  -> Json.fromString("array"),
        "items" -> item
      )
    case TMap(values) =>
      Json.obj(
        "type"   -> Json.fromString("map"),
        "values" -> values
      )
    case TRecord(name, namespace, aliases, doc, fields) =>
      val base: Json = Json.obj(
        "type"   -> Json.fromString("record"),
        "name"   -> Json.fromString(name),
        "fields" -> Json.arr(fields.map(field2Obj): _*)
      )
      val withNamespace = namespace.fold(base) { n =>
        base deepMerge Json.obj("namespace" -> Json.fromString(n))
      }
      val withAliases =
        if (aliases.isEmpty)
          withNamespace
        else
          withNamespace deepMerge Json.obj("aliases" -> Json.arr(aliases.map(Json.fromString): _*))
      val withDoc = doc.fold(withAliases) { f =>
        withAliases deepMerge Json.obj("doc" -> Json.fromString(f))
      }
      withDoc
    case TEnum(_, _, _, _, _) => ???
    case TUnion(options)      => Json.arr(options.toList: _*)
    case TFixed(name, _, _, size) =>
      Json.obj(
        "type" -> Json.fromString("fixed"),
        "name" -> Json.fromString(name),
        "size" -> Json.fromInt(size)
      )
  }
}
