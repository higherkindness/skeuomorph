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

import io.circe.Json
import iota.{TList => _, _}
import iota.TListK.:::

import higherkindness.droste.Algebra

import higherkindness.skeuomorph.uast.{derivation, mkInject, Delay}
import higherkindness.skeuomorph.uast.types._

package object openapi {

  type Types = TInt :::
    TLong :::
    TFloat :::
    TDouble :::
    TString :::
    TBoolean :::
    TDate :::
    TDateTime :::
    TPassword :::
    TRecord :::
    TList :::
    TUnion :::
    TNilK

  type Type[A] = CopK[Types, A]

  val InjInt: CopK.Inject[TInt, Type]           = mkInject[TInt, Types](0)
  val InjLong: CopK.Inject[TLong, Type]         = mkInject[TLong, Types](1)
  val InjFloat: CopK.Inject[TFloat, Type]       = mkInject[TFloat, Types](2)
  val InjDouble: CopK.Inject[TDouble, Type]     = mkInject[TDouble, Types](3)
  val InjString: CopK.Inject[TString, Type]     = mkInject[TString, Types](4)
  val InjBoolean: CopK.Inject[TBoolean, Type]   = mkInject[TBoolean, Types](5)
  val InjDate: CopK.Inject[TDate, Type]         = mkInject[TDate, Types](6)
  val InjDateTime: CopK.Inject[TDateTime, Type] = mkInject[TDateTime, Types](7)
  val InjPassword: CopK.Inject[TPassword, Type] = mkInject[TPassword, Types](8)
  val InjRecord: CopK.Inject[TRecord, Type]     = mkInject[TRecord, Types](9)
  val InjList: CopK.Inject[TList, Type]         = mkInject[TList, Types](10)
  val InjUnion: CopK.Inject[TUnion, Type]       = mkInject[TUnion, Types](11)
  implicit val openapiEq: Delay[Eq, Type]       = derivation.delayEqCopK[Types]
  implicit val openapiTraverse: Traverse[Type]  = derivation.copkTraverse[Types]

  object Type {

    def render: Algebra[Type, Json] = Algebra {
      case InjInt(_)      => Json.fromString("integer")
      case InjLong(_)     => Json.fromString("long")
      case InjFloat(_)    => Json.fromString("float")
      case InjDouble(_)   => Json.fromString("double")
      case InjString(_)   => Json.fromString("string")
      case InjBoolean(_)  => Json.fromString("boolean")
      case InjDate(_)     => Json.fromString("date")
      case InjDateTime(_) => Json.fromString("datetime")
      case InjPassword(_) => Json.fromString("password")
      case InjRecord(TRecord(name, properties)) =>
        Json.obj(
          name -> Json.obj(
            "type" -> Json.fromString("object"),
            "properties" -> Json.obj(
              properties.map(prop => FieldF.fieldName.get(prop) -> FieldF.fieldType.get(prop)): _*)
            //          "required"   -> Json.fromValues(required.map(Json.fromString))
          )
        )
      case InjList(TList(values)) =>
        Json.obj(
          "type"  -> Json.fromString("array"),
          "items" -> Json.obj("type" -> values)
        )
      case InjUnion(TUnion(cases)) =>
        Json.obj(
          "type" -> Json.fromString("string"),
          "enum" -> Json.fromValues(cases.toList)
        )
    }
  }

}
