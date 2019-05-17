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

import iota.{TList => _, _}
import iota.TListK.:::

import higherkindness.skeuomorph.uast.{derivation, mkInject, Delay}
import higherkindness.skeuomorph.uast.types._

package object mu {
  type Types = TNull :::
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
    TNilK

  type Type[A] = CopK[Types, A]

  implicit val InjNull: CopK.Inject[TNull, Type]             = mkInject[TNull, Types](0)
  implicit val InjDouble: CopK.Inject[TDouble, Type]         = mkInject[TDouble, Types](1)
  implicit val InjFloat: CopK.Inject[TFloat, Type]           = mkInject[TFloat, Types](2)
  implicit val InjInt: CopK.Inject[TInt, Type]               = mkInject[TInt, Types](3)
  implicit val InjLong: CopK.Inject[TLong, Type]             = mkInject[TLong, Types](4)
  implicit val InjBoolean: CopK.Inject[TBoolean, Type]       = mkInject[TBoolean, Types](5)
  implicit val InjString: CopK.Inject[TString, Type]         = mkInject[TString, Types](6)
  implicit val InjByteArray: CopK.Inject[TByteArray, Type]   = mkInject[TByteArray, Types](7)
  implicit val InjByte: CopK.Inject[TByte, Type]             = mkInject[TByte, Types](8)
  implicit val InjNamedType: CopK.Inject[TNamedType, Type]   = mkInject[TNamedType, Types](9)
  implicit val InjOption: CopK.Inject[TOption, Type]         = mkInject[TOption, Types](10)
  implicit val InjEither: CopK.Inject[TEither, Type]         = mkInject[TEither, Types](11)
  implicit val InjList: CopK.Inject[TList, Type]             = mkInject[TList, Types](12)
  implicit val InjMap: CopK.Inject[TMap, Type]               = mkInject[TMap, Types](13)
  implicit val InjGeneric: CopK.Inject[TGeneric, Type]       = mkInject[TGeneric, Types](14)
  implicit val InjUnion: CopK.Inject[TUnion, Type]           = mkInject[TUnion, Types](15)
  implicit val InjEnum: CopK.Inject[TEnum, Type]             = mkInject[TEnum, Types](16)
  implicit val InjRecord: CopK.Inject[TRecord, Type]         = mkInject[TRecord, Types](17)
  implicit val InjContaining: CopK.Inject[TContaining, Type] = mkInject[TContaining, Types](18)
  implicit val muEq: Delay[Eq, mu.Type]                      = derivation.delayEqCopK[Types]
  implicit val muTraverse: Traverse[Type]                    = derivation.copkTraverse[Types]
}
