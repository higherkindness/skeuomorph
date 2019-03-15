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

import io.circe.Json
import iota.{TList => _, _}
import iota.TListK.:::
import qq.droste.Algebra

import higherkindness.skeuomorph.uast.types._

package object openapi {

  type Type[A] = CopK[
    TInt :::
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
      TNilK,
    A
  ]

  val InjInt: CopK.Inject[TInt, Type]           = CopK.Inject[TInt, Type]
  val InjLong: CopK.Inject[TLong, Type]         = CopK.Inject[TLong, Type]
  val InjFloat: CopK.Inject[TFloat, Type]       = CopK.Inject[TFloat, Type]
  val InjDouble: CopK.Inject[TDouble, Type]     = CopK.Inject[TDouble, Type]
  val InjString: CopK.Inject[TString, Type]     = CopK.Inject[TString, Type]
  val InjBoolean: CopK.Inject[TBoolean, Type]   = CopK.Inject[TBoolean, Type]
  val InjDate: CopK.Inject[TDate, Type]         = CopK.Inject[TDate, Type]
  val InjDateTime: CopK.Inject[TDateTime, Type] = CopK.Inject[TDateTime, Type]
  val InjPassword: CopK.Inject[TPassword, Type] = CopK.Inject[TPassword, Type]
  val InjRecord: CopK.Inject[TRecord, Type]     = CopK.Inject[TRecord, Type]
  val InjList: CopK.Inject[TList, Type]         = CopK.Inject[TList, Type]
  val InjUnion: CopK.Inject[TUnion, Type]       = CopK.Inject[TUnion, Type]

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
