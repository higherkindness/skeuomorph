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
package uast

import higherkindness.skeuomorph.uast.types._

import cats.~>
import cats.implicits._
import cats.data.NonEmptyList

import qq.droste._
import qq.droste.syntax.compose._

import org.scalacheck._
import org.scalacheck.cats.implicits._

object arbitraries {
  implicit val arbField: Delay[Arbitrary, Field] =
    λ[Arbitrary ~> (Arbitrary ∘ Field)#λ](aa => Arbitrary((instances.nonEmptyString, aa.arbitrary).mapN(Field.apply)))
  implicit val arbitraryTNull: Delay[Arbitrary, TNull] =
    λ[Arbitrary ~> (Arbitrary ∘ TNull)#λ](aa => Arbitrary(Gen.const(TNull())))
  implicit val arbitraryTBoolean: Delay[Arbitrary, TBoolean] =
    λ[Arbitrary ~> (Arbitrary ∘ TBoolean)#λ](aa => Arbitrary(Gen.const(TBoolean())))
  implicit val arbitraryTInt: Delay[Arbitrary, TInt] =
    λ[Arbitrary ~> (Arbitrary ∘ TInt)#λ](aa => Arbitrary(Gen.const(TInt())))
  implicit val arbitraryTLong: Delay[Arbitrary, TLong] =
    λ[Arbitrary ~> (Arbitrary ∘ TLong)#λ](aa => Arbitrary(Gen.const(TLong())))
  implicit val arbitraryTFloat: Delay[Arbitrary, TFloat] =
    λ[Arbitrary ~> (Arbitrary ∘ TFloat)#λ](aa => Arbitrary(Gen.const(TFloat())))
  implicit val arbitraryTDouble: Delay[Arbitrary, TDouble] =
    λ[Arbitrary ~> (Arbitrary ∘ TDouble)#λ](aa => Arbitrary(Gen.const(TDouble())))
  implicit val arbitraryTString: Delay[Arbitrary, TString] =
    λ[Arbitrary ~> (Arbitrary ∘ TString)#λ](aa => Arbitrary(Gen.const(TString())))
  implicit val arbitraryTDate: Delay[Arbitrary, TDate] =
    λ[Arbitrary ~> (Arbitrary ∘ TDate)#λ](aa => Arbitrary(Gen.const(TDate())))
  implicit val arbitraryTDateTime: Delay[Arbitrary, TDateTime] =
    λ[Arbitrary ~> (Arbitrary ∘ TDateTime)#λ](aa => Arbitrary(Gen.const(TDateTime())))
  implicit val arbitraryTPassword: Delay[Arbitrary, TPassword] =
    λ[Arbitrary ~> (Arbitrary ∘ TPassword)#λ](aa => Arbitrary(Gen.const(TPassword())))
  implicit val arbitraryTByte: Delay[Arbitrary, TByte] =
    λ[Arbitrary ~> (Arbitrary ∘ TByte)#λ](aa => Arbitrary(Gen.const(TByte())))
  implicit val arbitraryTByteArray: Delay[Arbitrary, TByteArray] =
    λ[Arbitrary ~> (Arbitrary ∘ TByteArray)#λ](aa => Arbitrary(Gen.const(TByteArray())))
  implicit val arbitraryTNamedType: Delay[Arbitrary, TNamedType] =
    λ[Arbitrary ~> (Arbitrary ∘ TNamedType)#λ](aa => Arbitrary(instances.nonEmptyString.map(TNamedType.apply)))
  implicit val arbitraryTMap: Delay[Arbitrary, TMap] =
    λ[Arbitrary ~> (Arbitrary ∘ TMap)#λ](aa => Arbitrary((aa.arbitrary, aa.arbitrary).mapN(TMap.apply)))
  implicit val arbitraryTNamedFixed: Delay[Arbitrary, TNamedFixed] = λ[Arbitrary ~> (Arbitrary ∘ TNamedFixed)#λ](aa =>
    Arbitrary((Arbitrary.arbString.arbitrary, Arbitrary.arbInt.arbitrary).mapN(TNamedFixed.apply)))
  implicit val arbitraryTOption: Delay[Arbitrary, TOption] =
    λ[Arbitrary ~> (Arbitrary ∘ TOption)#λ](aa => Arbitrary((aa.arbitrary).map(TOption.apply)))
  implicit val arbitraryTEither: Delay[Arbitrary, TEither] =
    λ[Arbitrary ~> (Arbitrary ∘ TEither)#λ](aa => Arbitrary((aa.arbitrary, aa.arbitrary).mapN(TEither.apply)))
  implicit val arbitraryTList: Delay[Arbitrary, TList] =
    λ[Arbitrary ~> (Arbitrary ∘ TList)#λ](aa => Arbitrary((aa.arbitrary).map(TList.apply)))
  implicit val arbitraryTGeneric: Delay[Arbitrary, TGeneric] = λ[Arbitrary ~> (Arbitrary ∘ TGeneric)#λ](aa =>
    Arbitrary((aa.arbitrary, Gen.listOf(aa.arbitrary)).mapN(TGeneric.apply)))
  implicit val arbitraryFieldF: Delay[Arbitrary, FieldF] =
    λ[Arbitrary ~> (Arbitrary ∘ FieldF)#λ](aa =>
      Arbitrary((instances.nonEmptyString, aa.arbitrary).mapN(FieldF.SimpleField)))
  implicit val arbitraryTRecord: Delay[Arbitrary, TRecord] = λ[Arbitrary ~> (Arbitrary ∘ TRecord)#λ](aa =>
    Arbitrary((instances.nonEmptyString, Gen.listOf(arbitraryFieldF(aa).arbitrary)).mapN(TRecord.apply)))
  implicit val arbitraryTEnum: Delay[Arbitrary, TEnum] = λ[Arbitrary ~> (Arbitrary ∘ TEnum)#λ](aa =>
    Arbitrary((instances.nonEmptyString, Gen.listOf(instances.nonEmptyString)).mapN(TEnum.apply)))
  implicit val arbitraryTUnion: Delay[Arbitrary, TUnion] = λ[Arbitrary ~> (Arbitrary ∘ TUnion)#λ](aa =>
    Arbitrary((Gen.nonEmptyListOf(aa.arbitrary)).map(a => TUnion(NonEmptyList.fromListUnsafe(a)))))
  implicit val arbitraryTContaining: Delay[Arbitrary, TContaining] = λ[Arbitrary ~> (Arbitrary ∘ TContaining)#λ](aa =>
    Arbitrary(Gen.nonEmptyListOf(aa.arbitrary).map(TContaining.apply)))
  implicit val arbitraryTFileDescriptor: Delay[Arbitrary, TFileDescriptor] =
    λ[Arbitrary ~> (Arbitrary ∘ TFileDescriptor)#λ](
      aa =>
        Arbitrary(
          (Gen.listOf(aa.arbitrary), instances.nonEmptyString, instances.nonEmptyString).mapN(TFileDescriptor.apply)))

}
