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

import iota.{TList => _, _}
import iota.TListK.:::
import uast.types._

package object mu {
  type Type[A] = CopK[
    TNull :::
      TDouble :::
      TFloat :::
      TInt :::
      TLong :::
      TBoolean :::
      TString :::
      TByteArray :::
      TByte :::
      TNamedType :::
      TOption :::
      TEither :::
      TList :::
      TMap :::
      TGeneric :::
      TUnion :::
      TEnum :::
      TRecord :::
      TContaining :::
      TNilK,
    A]

  implicit val InjNull: CopK.Inject[TNull, Type]             = CopK.Inject[TNull, Type]
  implicit val InjDouble: CopK.Inject[TDouble, Type]         = CopK.Inject[TDouble, Type]
  implicit val InjFloat: CopK.Inject[TFloat, Type]           = CopK.Inject[TFloat, Type]
  implicit val InjInt: CopK.Inject[TInt, Type]               = CopK.Inject[TInt, Type]
  implicit val InjLong: CopK.Inject[TLong, Type]             = CopK.Inject[TLong, Type]
  implicit val InjBoolean: CopK.Inject[TBoolean, Type]       = CopK.Inject[TBoolean, Type]
  implicit val InjString: CopK.Inject[TString, Type]         = CopK.Inject[TString, Type]
  implicit val InjByteArray: CopK.Inject[TByteArray, Type]   = CopK.Inject[TByteArray, Type]
  implicit val InjByte: CopK.Inject[TByte, Type]             = CopK.Inject[TByte, Type]
  implicit val InjNamedType: CopK.Inject[TNamedType, Type]   = CopK.Inject[TNamedType, Type]
  implicit val InjOption: CopK.Inject[TOption, Type]         = CopK.Inject[TOption, Type]
  implicit val InjEither: CopK.Inject[TEither, Type]         = CopK.Inject[TEither, Type]
  implicit val InjList: CopK.Inject[TList, Type]             = CopK.Inject[TList, Type]
  implicit val InjMap: CopK.Inject[TMap, Type]               = CopK.Inject[TMap, Type]
  implicit val InjGeneric: CopK.Inject[TGeneric, Type]       = CopK.Inject[TGeneric, Type]
  implicit val InjUnion: CopK.Inject[TUnion, Type]           = CopK.Inject[TUnion, Type]
  implicit val InjEnum: CopK.Inject[TEnum, Type]             = CopK.Inject[TEnum, Type]
  implicit val InjRecord: CopK.Inject[TRecord, Type]         = CopK.Inject[TRecord, Type]
  implicit val InjContaining: CopK.Inject[TContaining, Type] = CopK.Inject[TContaining, Type]

}
