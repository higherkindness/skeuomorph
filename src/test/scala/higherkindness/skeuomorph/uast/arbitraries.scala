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
  implicit val arbField: Delay[Arbitrary, Field] = new (Arbitrary ~> (Arbitrary ∘ Field)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[Field[A]] =
      Arbitrary((instances.nonEmptyString, aa.arbitrary).mapN(Field.apply))
  }
  implicit val arbitraryTNull: Delay[Arbitrary, TNull] = new (Arbitrary ~> (Arbitrary ∘ TNull)#λ) {
    def apply[A](a: Arbitrary[A]): Arbitrary[TNull[A]] = Arbitrary(Gen.const(TNull[A]()))
  }
  implicit val arbitraryTBoolean: Delay[Arbitrary, TBoolean] = new (Arbitrary ~> (Arbitrary ∘ TBoolean)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TBoolean[A]] = Arbitrary(Gen.const(TBoolean[A]()))
  }
  implicit val arbitraryTInt: Delay[Arbitrary, TInt] = new (Arbitrary ~> (Arbitrary ∘ TInt)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TInt[A]] = Arbitrary(Gen.const(TInt[A]()))
  }
  implicit val arbitraryTLong: Delay[Arbitrary, TLong] = new (Arbitrary ~> (Arbitrary ∘ TLong)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TLong[A]] = Arbitrary(Gen.const(TLong[A]()))
  }
  implicit val arbitraryTFloat: Delay[Arbitrary, TFloat] = new (Arbitrary ~> (Arbitrary ∘ TFloat)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TFloat[A]] = Arbitrary(Gen.const(TFloat[A]()))
  }
  implicit val arbitraryTDouble: Delay[Arbitrary, TDouble] = new (Arbitrary ~> (Arbitrary ∘ TDouble)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TDouble[A]] = Arbitrary(Gen.const(TDouble[A]()))
  }
  implicit val arbitraryTString: Delay[Arbitrary, TString] = new (Arbitrary ~> (Arbitrary ∘ TString)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TString[A]] = Arbitrary(Gen.const(TString[A]()))
  }
  implicit val arbitraryTDate: Delay[Arbitrary, TDate] = new (Arbitrary ~> (Arbitrary ∘ TDate)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TDate[A]] = Arbitrary(Gen.const(TDate[A]()))
  }
  implicit val arbitraryTDateTime: Delay[Arbitrary, TDateTime] = new (Arbitrary ~> (Arbitrary ∘ TDateTime)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TDateTime[A]] = Arbitrary(Gen.const(TDateTime[A]()))
  }
  implicit val arbitraryTPassword: Delay[Arbitrary, TPassword] = new (Arbitrary ~> (Arbitrary ∘ TPassword)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TPassword[A]] = Arbitrary(Gen.const(TPassword[A]()))
  }
  implicit val arbitraryTByte: Delay[Arbitrary, TByte] = new (Arbitrary ~> (Arbitrary ∘ TByte)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TByte[A]] = Arbitrary(Gen.const(TByte[A]()))
  }
  implicit val arbitraryTByteArray: Delay[Arbitrary, TByteArray] = new (Arbitrary ~> (Arbitrary ∘ TByteArray)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TByteArray[A]] = Arbitrary(Gen.const(TByteArray[A]()))
  }
  implicit val arbitraryTNamedType: Delay[Arbitrary, TNamedType] = new (Arbitrary ~> (Arbitrary ∘ TNamedType)#λ) {
    def apply[A](a: Arbitrary[A]): Arbitrary[TNamedType[A]] = Arbitrary(instances.nonEmptyString.map(TNamedType.apply))
  }
  implicit val arbitraryTMap: Delay[Arbitrary, TMap] = new (Arbitrary ~> (Arbitrary ∘ TMap)#λ) {
    def apply[A](a: Arbitrary[A]): Arbitrary[TMap[A]] = Arbitrary((a.arbitrary, a.arbitrary).mapN(TMap.apply))
  }
  implicit val arbitraryTNamedFixed: Delay[Arbitrary, TNamedFixed] = new (Arbitrary ~> (Arbitrary ∘ TNamedFixed)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TNamedFixed[A]] =
      Arbitrary((Arbitrary.arbString.arbitrary, Arbitrary.arbInt.arbitrary).mapN(TNamedFixed.apply))
  }
  implicit val arbitraryTOption: Delay[Arbitrary, TOption] = new (Arbitrary ~> (Arbitrary ∘ TOption)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TOption[A]] = Arbitrary((aa.arbitrary).map(TOption.apply))
  }
  implicit val arbitraryTEither: Delay[Arbitrary, TEither] = new (Arbitrary ~> (Arbitrary ∘ TEither)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TEither[A]] = Arbitrary((aa.arbitrary, aa.arbitrary).mapN(TEither.apply))
  }
  implicit val arbitraryTList: Delay[Arbitrary, TList] = new (Arbitrary ~> (Arbitrary ∘ TList)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TList[A]] = Arbitrary((aa.arbitrary).map(TList.apply))
  }
  implicit val arbitraryTGeneric: Delay[Arbitrary, TGeneric] = new (Arbitrary ~> (Arbitrary ∘ TGeneric)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TGeneric[A]] =
      Arbitrary((aa.arbitrary, Gen.listOf(aa.arbitrary)).mapN(TGeneric.apply))
  }
  implicit val arbitraryFieldF: Delay[Arbitrary, FieldF] =
    λ[Arbitrary ~> (Arbitrary ∘ FieldF)#λ](aa =>
      Arbitrary((instances.nonEmptyString, aa.arbitrary).mapN(FieldF.SimpleField)))
  implicit val arbitraryTRecord: Delay[Arbitrary, TRecord] = new (Arbitrary ~> (Arbitrary ∘ TRecord)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TRecord[A]] =
      Arbitrary((instances.nonEmptyString, Gen.listOf(arbitraryFieldF(aa).arbitrary)).mapN(TRecord.apply))
  }
  implicit val arbitraryTEnum: Delay[Arbitrary, TEnum] = new (Arbitrary ~> (Arbitrary ∘ TEnum)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TEnum[A]] =
      Arbitrary((instances.nonEmptyString, Gen.listOf(instances.nonEmptyString)).mapN(TEnum.apply))
  }
  implicit val arbitraryTUnion: Delay[Arbitrary, TUnion] = new (Arbitrary ~> (Arbitrary ∘ TUnion)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TUnion[A]] =
      Arbitrary((Gen.nonEmptyListOf(aa.arbitrary)).map(a => TUnion(NonEmptyList.fromListUnsafe(a))))
  }
  implicit val arbitraryTContaining: Delay[Arbitrary, TContaining] = new (Arbitrary ~> (Arbitrary ∘ TContaining)#λ) {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TContaining[A]] =
      Arbitrary(Gen.nonEmptyListOf(aa.arbitrary).map(TContaining.apply))
  }
  implicit val arbitraryTFileDescriptor: Delay[Arbitrary, TFileDescriptor] =
    new (Arbitrary ~> (Arbitrary ∘ TFileDescriptor)#λ) {
      def apply[A](aa: Arbitrary[A]): Arbitrary[TFileDescriptor[A]] =
        Arbitrary(
          (Gen.listOf(aa.arbitrary), instances.nonEmptyString, instances.nonEmptyString).mapN(TFileDescriptor.apply))
    }
}
