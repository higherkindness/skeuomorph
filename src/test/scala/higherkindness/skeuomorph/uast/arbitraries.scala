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

import higherkindness.skeuomorph.compdata._
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.protobuf.TProtoEnum

import cats.implicits._
import cats.data.NonEmptyList

import org.scalacheck._
import org.scalacheck.cats.implicits._

object arbitraries {
  implicit val arbField: Delay[Arbitrary, Field] =
    new Delay[Arbitrary, Field] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary((instances.nonEmptyString, aa.arbitrary).mapN(Field.apply))
    }
  implicit val arbitraryTNull: Delay[Arbitrary, TNull] =
    new Delay[Arbitrary, TNull] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TNull())) }
  implicit val arbitraryTBoolean: Delay[Arbitrary, TBoolean] =
    new Delay[Arbitrary, TBoolean] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TBoolean())) }
  implicit val arbitraryTInt: Delay[Arbitrary, TInt] =
    new Delay[Arbitrary, TInt] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TInt())) }
  implicit val arbitraryTLong: Delay[Arbitrary, TLong] =
    new Delay[Arbitrary, TLong] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TLong())) }
  implicit val arbitraryTFloat: Delay[Arbitrary, TFloat] =
    new Delay[Arbitrary, TFloat] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TFloat())) }
  implicit val arbitraryTDouble: Delay[Arbitrary, TDouble] =
    new Delay[Arbitrary, TDouble] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TDouble())) }
  implicit val arbitraryTString: Delay[Arbitrary, TString] =
    new Delay[Arbitrary, TString] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TString())) }
  implicit val arbitraryTDate: Delay[Arbitrary, TDate] =
    new Delay[Arbitrary, TDate] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TDate())) }
  implicit val arbitraryTDateTime: Delay[Arbitrary, TDateTime] =
    new Delay[Arbitrary, TDateTime] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TDateTime())) }
  implicit val arbitraryTPassword: Delay[Arbitrary, TPassword] =
    new Delay[Arbitrary, TPassword] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TPassword())) }
  implicit val arbitraryTByte: Delay[Arbitrary, TByte] =
    new Delay[Arbitrary, TByte] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TByte())) }
  implicit val arbitraryTByteArray: Delay[Arbitrary, TByteArray] =
    new Delay[Arbitrary, TByteArray] { def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.const(TByteArray())) }
  implicit val arbitraryTNamedType: Delay[Arbitrary, TNamedType] =
    new Delay[Arbitrary, TNamedType] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary(instances.nonEmptyString.map(TNamedType.apply))
    }
  implicit val arbitraryTMap: Delay[Arbitrary, TMap] =
    new Delay[Arbitrary, TMap] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary((aa.arbitrary, aa.arbitrary).mapN(TMap.apply))
    }
  implicit val arbitraryTNamedFixed: Delay[Arbitrary, TNamedFixed] =
    new Delay[Arbitrary, TNamedFixed] {
      def apply[A](aa: Arbitrary[A]) =
        Arbitrary((Arbitrary.arbString.arbitrary, Arbitrary.arbInt.arbitrary).mapN(TNamedFixed.apply))
    }
  implicit val arbitraryTOption: Delay[Arbitrary, TOption] =
    new Delay[Arbitrary, TOption] { def apply[A](aa: Arbitrary[A]) = Arbitrary((aa.arbitrary).map(TOption.apply)) }
  implicit val arbitraryTEither: Delay[Arbitrary, TEither] =
    new Delay[Arbitrary, TEither] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary((aa.arbitrary, aa.arbitrary).mapN(TEither.apply))
    }
  implicit val arbitraryTList: Delay[Arbitrary, TList] =
    new Delay[Arbitrary, TList] { def apply[A](aa: Arbitrary[A]) = Arbitrary((aa.arbitrary).map(TList.apply)) }
  implicit val arbitraryTGeneric: Delay[Arbitrary, TGeneric] =
    new Delay[Arbitrary, TGeneric] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary((aa.arbitrary, Gen.listOf(aa.arbitrary)).mapN(TGeneric.apply))
    }
  implicit val arbitraryFieldF: Delay[Arbitrary, FieldF] =
    new Delay[Arbitrary, FieldF] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary((instances.nonEmptyString, aa.arbitrary).mapN(FieldF.SimpleField))
    }
  implicit val arbitraryTRecord: Delay[Arbitrary, TRecord] =
    new Delay[Arbitrary, TRecord] {
      def apply[A](aa: Arbitrary[A]) =
        Arbitrary((instances.nonEmptyString, Gen.listOf(arbitraryFieldF(aa).arbitrary)).mapN(TRecord.apply))
    }
  implicit val arbitraryTEnum: Delay[Arbitrary, TEnum] =
    new Delay[Arbitrary, TEnum] {
      def apply[A](aa: Arbitrary[A]) =
        Arbitrary((instances.nonEmptyString, Gen.listOf(instances.nonEmptyString)).mapN(TEnum.apply))
    }
  implicit val arbitraryTUnion: Delay[Arbitrary, TUnion] =
    new Delay[Arbitrary, TUnion] {
      def apply[A](aa: Arbitrary[A]) =
        Arbitrary((Gen.nonEmptyListOf(aa.arbitrary)).map(a => TUnion(NonEmptyList.fromListUnsafe(a))))
    }
  implicit val arbitraryTContaining: Delay[Arbitrary, TContaining] =
    new Delay[Arbitrary, TContaining] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary(Gen.nonEmptyListOf(aa.arbitrary).map(TContaining.apply))
    }
  implicit val arbitraryTFileDescriptor: Delay[Arbitrary, TFileDescriptor] =
    new Delay[Arbitrary, TFileDescriptor] {
      def apply[A](aa: Arbitrary[A]): Arbitrary[TFileDescriptor[A]] =
        Arbitrary(
          (Gen.listOf(aa.arbitrary), instances.nonEmptyString, instances.nonEmptyString).mapN(TFileDescriptor.apply))
    }
  implicit val arbitraryTOneOf: Delay[Arbitrary, TOneOf] = new Delay[Arbitrary, TOneOf] {
    def apply[A](aa: Arbitrary[A]): Arbitrary[TOneOf[A]] =
      Arbitrary(
        (instances.nonEmptyString, Gen.nonEmptyListOf(arbitraryFieldF(aa).arbitrary).map(NonEmptyList.fromListUnsafe))
          .mapN(TOneOf.apply))

  }
  implicit val arbitraryTProtoEnum: Delay[Arbitrary, TProtoEnum] =
    new Delay[Arbitrary, TProtoEnum] {
      def apply[A](aa: Arbitrary[A]): Arbitrary[TProtoEnum[A]] =
        Arbitrary((
          instances.nonEmptyString,
          Gen.listOf((instances.nonEmptyString, Arbitrary.arbInt.arbitrary).tupled),
          Gen.listOf(instances.arbOptionValue.arbitrary),
          Gen.listOf((instances.nonEmptyString, Arbitrary.arbInt.arbitrary).tupled)
        ).mapN((name, fields, options, aliases) =>
          Ann(TEnum(name, fields.map(_._1)), protobuf.annotations.EnumAnnotation(fields.map(_._2), options, aliases))))
    }

  def arbitraryAnnotated[F[_], E](implicit F: Delay[Arbitrary, F], E: Arbitrary[E]): Delay[Arbitrary, Ann[F, E, ?]] =
    new Delay[Arbitrary, Ann[F, E, ?]] {
      def apply[A](aa: Arbitrary[A]) = Arbitrary((F(aa).arbitrary, E.arbitrary).mapN(Ann.apply))
    }

}
