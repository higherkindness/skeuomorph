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

import cats.{Eq, Traverse}
import cats.data.NonEmptyList
import io.circe.Json
import iota.{TList => _, _}
import iota.TListK.:::
import org.apache.avro.Schema
import org.apache.avro.Schema.{Type => SType}
import higherkindness.droste.{Algebra, Coalgebra}
import higherkindness.skeuomorph.uast._
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.avro.types._
import higherkindness.skeuomorph.compdata.Ann

import scala.collection.JavaConverters._

package object avro {

  type TAvroRecord[A] = Ann[TRecord, AvroMetadata, A]
  implicit val avroRecordTraverse: Traverse[TAvroRecord] = Ann.traverse[TRecord, AvroMetadata]

  type Types = TNull :::
    TBoolean :::
    TInt :::
    TLong :::
    TFloat :::
    TDouble :::
    TByteArray :::
    TString :::
    TList :::
    TMap :::
    TAvroRecord :::
    TEnum :::
    TUnion :::
    TNamedType :::
    TNamedFixed :::
    TNilK

  type Type[A] = CopK[Types, A]

  implicit val avroTraverse: Traverse[avro.Type]             = derivation.copkTraverse[Types]
  implicit val InjNull: CopK.Inject[TNull, Type]             = mkInject[TNull, Types](0)
  implicit val InjBoolean: CopK.Inject[TBoolean, Type]       = mkInject[TBoolean, Types](1)
  implicit val InjInt: CopK.Inject[TInt, Type]               = mkInject[TInt, Types](2)
  implicit val InjLong: CopK.Inject[TLong, Type]             = mkInject[TLong, Types](3)
  implicit val InjFloat: CopK.Inject[TFloat, Type]           = mkInject[TFloat, Types](4)
  implicit val InjDouble: CopK.Inject[TDouble, Type]         = mkInject[TDouble, Types](5)
  implicit val InjByteArray: CopK.Inject[TByteArray, Type]   = mkInject[TByteArray, Types](6)
  implicit val InjString: CopK.Inject[TString, Type]         = mkInject[TString, Types](7)
  implicit val InjList: CopK.Inject[TList, Type]             = mkInject[TList, Types](8)
  implicit val InjMap: CopK.Inject[TMap, Type]               = mkInject[TMap, Types](9)
  implicit val InjAvroRecord: CopK.Inject[TAvroRecord, Type] = mkInject[TAvroRecord, Types](10)
  implicit val InjEnum: CopK.Inject[TEnum, Type]             = mkInject[TEnum, Types](11)
  implicit val InjUnion: CopK.Inject[TUnion, Type]           = mkInject[TUnion, Types](12)
  implicit val InjNamedType: CopK.Inject[TNamedType, Type]   = mkInject[TNamedType, Types](13)
  implicit val InjFixed: CopK.Inject[TNamedFixed, Type]      = mkInject[TNamedFixed, Types](14)
  implicit val eqTAvroRecord: Delay[Eq, TAvroRecord]         = Ann.delayEq[TRecord, AvroMetadata]
  implicit val avroEq: Delay[Eq, avro.Type]                  = derivation.delayEqCopK[Types]

  def avroRecord[F[α] <: ACopK[α], A](
      name: String,
      namespace: Option[String],
      aliases: List[String],
      doc: Option[String],
      fields: List[FieldF[A]]
  ): Type[A] =
    InjAvroRecord[A](
      Ann(
        TRecord(name, fields),
        AvroMetadata.AMList(
          List(AvroMetadata.NameSpace(namespace), AvroMetadata.Aliases(aliases), AvroMetadata.Doc(doc)))))

  object Type {

    def order2Order(avroO: Schema.Field.Order): Order = avroO match {
      case Schema.Field.Order.ASCENDING  => Order.Ascending
      case Schema.Field.Order.DESCENDING => Order.Descending
      case Schema.Field.Order.IGNORE     => Order.Ignore
    }

    def fromAvro: Coalgebra[Type, Schema] = Coalgebra { sch =>
      sch.getType match {
        case SType.NULL    => `null`[Type, Schema]
        case SType.BOOLEAN => boolean[Type, Schema]
        case SType.INT     => int[Type, Schema]
        case SType.LONG    => long[Type, Schema]
        case SType.FLOAT   => float[Type, Schema]
        case SType.DOUBLE  => double[Type, Schema]
        case SType.BYTES   => byteArray[Type, Schema]
        case SType.STRING  => string[Type, Schema]
        case SType.ARRAY   => list[Type, Schema](sch.getElementType)
        case SType.MAP     => map[Type, Schema](Schema.create(SType.STRING), sch.getValueType)
        case SType.RECORD =>
          avroRecord[Type, Schema](
            sch.getName,
            Option(sch.getNamespace),
            sch.getAliases.asScala.toList,
            Option(sch.getDoc),
            sch.getFields.asScala.toList.map(field2Field)
          )
        case SType.ENUM  => enum[Type, Schema](sch.getName, sch.getAliases.asScala.toList)
        case SType.UNION => union[Type, Schema](NonEmptyList.fromListUnsafe(sch.getTypes.asScala.toList))
        case SType.FIXED => fixed[Type, Schema](sch.getName, sch.getFixedSize)
      }
    }

    def field2Field(avroField: Schema.Field): FieldF[Schema] = FieldF.AvroField(
      avroField.name,
      avroField.aliases.asScala.toList,
      Option(avroField.doc),
      Option(order2Order(avroField.order)),
      avroField.schema
    )

    private[this] def field2Obj(f: FieldF[Json]): Json = f match {
      case FieldF.InjSimpleField(f) =>
        Json.obj(
          "name" -> Json.fromString(f.name),
          "type" -> f.tpe
        )
      case FieldF.InjAvroField(Ann(f, _)) =>
        Json.obj(
          "name" -> Json.fromString(f.name),
          "type" -> f.tpe
        )
      case FieldF.InjProtobufField(Ann(f, _)) =>
        Json.obj(
          "name" -> Json.fromString(f.name),
          "type" -> f.tpe
        )
    }

    def toJson: Algebra[Type, Json] = Algebra {
      case InjNull(_)      => Json.Null
      case InjBoolean(_)   => Json.fromString("boolean")
      case InjInt(_)       => Json.fromString("integer")
      case InjLong(_)      => Json.fromString("long")
      case InjFloat(_)     => Json.fromString("float")
      case InjDouble(_)    => Json.fromString("double")
      case InjByteArray(_) => Json.fromString("bytes")
      case InjString(_)    => Json.fromString("string")
      case InjMap(TMap(keys, values)) =>
        Json.obj(
          "type" -> Json.fromString("map"),
          "items" -> keys.asArray
            .flatMap(k => values.asArray.map(v => k.map(_.toString).zip(v)))
            .fold(Json.arr(Seq.empty[Json]: _*))(v => Json.arr(v.toSeq.map(Json.obj(_)): _*))
        )
      case InjAvroRecord(Ann(TRecord(name, fields), _)) =>
        Json.obj(
          "type"   -> Json.fromString("record"),
          "name"   -> Json.fromString(name),
          "fields" -> Json.arr(fields.map(field2Obj): _*))
      case InjEnum(TEnum(_, _))      => ???
      case InjUnion(TUnion(options)) => Json.arr(options.toList: _*)
      case InjFixed(TNamedFixed(name, size)) =>
        Json.obj(
          "type" -> Json.fromString("fixed"),
          "name" -> Json.fromString(name),
          "size" -> Json.fromInt(size)
        )
    }
  }
}
