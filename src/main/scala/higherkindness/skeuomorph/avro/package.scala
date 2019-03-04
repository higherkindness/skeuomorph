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
import io.circe.Json
import iota.{TListK => _, _}
import iota.TListK.:::
import org.apache.avro.Schema
import org.apache.avro.Schema.{Type => SType}
import qq.droste.{Algebra, Coalgebra}
import uast.types._

import scala.collection.JavaConverters._

package object avro {

  type Type[A] = CopK[
    TNull :::
      TBoolean :::
      TInt :::
      TLong :::
      TFloat :::
      TDouble :::
      TByteArray :::
      TString :::
      TMap :::
      TRecord :::
      TEnum :::
      TUnion :::
      TNamedFixed :::
      TNilK,
    A
  ]

  implicit val InjNull: CopK.Inject[TNull, Type]           = CopK.Inject[TNull, Type]
  implicit val InjBoolean: CopK.Inject[TBoolean, Type]     = CopK.Inject[TBoolean, Type]
  implicit val InjInt: CopK.Inject[TInt, Type]             = CopK.Inject[TInt, Type]
  implicit val InjLong: CopK.Inject[TLong, Type]           = CopK.Inject[TLong, Type]
  implicit val InjFloat: CopK.Inject[TFloat, Type]         = CopK.Inject[TFloat, Type]
  implicit val InjDouble: CopK.Inject[TDouble, Type]       = CopK.Inject[TDouble, Type]
  implicit val InjByteArray: CopK.Inject[TByteArray, Type] = CopK.Inject[TByteArray, Type]
  implicit val InjString: CopK.Inject[TString, Type]       = CopK.Inject[TString, Type]
  implicit val InjMap: CopK.Inject[TMap, Type]             = CopK.Inject[TMap, Type]
  implicit val InjRecord: CopK.Inject[TRecord, Type]       = CopK.Inject[TRecord, Type]
  implicit val InjEnum: CopK.Inject[TEnum, Type]           = CopK.Inject[TEnum, Type]
  implicit val InjUnion: CopK.Inject[TUnion, Type]         = CopK.Inject[TUnion, Type]
  implicit val InjFixed: CopK.Inject[TNamedFixed, Type]    = CopK.Inject[TNamedFixed, Type]

  object Type {

    def fromAvro: Coalgebra[Type, Schema] = Coalgebra { sch =>
      sch.getType match {
        case t if t == SType.NULL    => `null`[Type, Schema]
        case t if t == SType.BOOLEAN => boolean[Type, Schema]
        case t if t == SType.INT     => int[Type, Schema]
        case t if t == SType.LONG    => long[Type, Schema]
        case t if t == SType.FLOAT   => float[Type, Schema]
        case t if t == SType.DOUBLE  => double[Type, Schema]
        case t if t == SType.BYTES   => byteArray[Type, Schema]
        case t if t == SType.STRING  => string[Type, Schema]
        case t if t == SType.MAP     => map[Type, Schema](Schema.create(SType.STRING), sch.getValueType)
        case t if t == SType.ENUM    => enum[Type, Schema](sch.getName, sch.getAliases.asScala.toList)
        case t if t == SType.UNION   => union[Type, Schema](NonEmptyList.fromListUnsafe(sch.getTypes.asScala.toList))
        case t if t == SType.FIXED   => fixed[Type, Schema](sch.getName, sch.getFixedSize)
      }
    }

    private[this] def field2Obj(f: Field[Json]): Json =
      Json.obj(
        "name" -> Json.fromString(f.name),
        "type" -> f.tpe
      )

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
      case InjRecord(TRecord(name, fields)) =>
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
