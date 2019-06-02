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
import cats.implicits._

import iota.{TList => _, _}
import iota.TListK.:::

import higherkindness.skeuomorph.uast.{derivation, mkInject}
import higherkindness.skeuomorph.uast.{:<<:, ACopK, Delay}
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.compdata.Ann

package object protobuf {

  // annotations used for adding extra data to uast types
  object annotations {
    final case class `32`()
    object `32` {
      implicit val eq: Eq[`32`] = Eq.fromUniversalEquals
    }
    final case class `64`()
    object `64` {
      implicit val eq: Eq[`64`] = Eq.fromUniversalEquals
    }
    final case class Signed()
    object Signed {
      implicit val eq: Eq[Signed] = Eq.fromUniversalEquals
    }
    final case class Unsigned()
    object Unsigned {
      implicit val eq: Eq[Unsigned] = Eq.fromUniversalEquals
    }
    final case class Fixed()
    object Fixed {
      implicit val eq: Eq[Fixed] = Eq.fromUniversalEquals
    }
    final case class Reserved(reserved: List[List[String]])
    object Reserved {
      implicit val eq: Eq[Reserved] = Eq.fromUniversalEquals
    }
    final case class EnumAnnotation(
        fieldNumbers: List[Int],
        options: List[(String, String)],
        aliases: List[(String, Int)])
    object EnumAnnotation {
      implicit val eq: Eq[EnumAnnotation] = Eq.fromUniversalEquals
    }
  }

  type TProtoEnum[A] = Ann[TEnum, annotations.EnumAnnotation, A]
  implicit val eqTProtoEnum: Delay[Eq, TProtoEnum]      = Ann.delayEq[TEnum, annotations.EnumAnnotation]
  implicit val traverseTProtoEnum: Traverse[TProtoEnum] = Ann.traverse[TEnum, annotations.EnumAnnotation]

  type TInt32[A] = Ann[TInt, annotations.`32`, A]
  implicit val eqTInt32: Delay[Eq, TInt32]      = Ann.delayEq[TInt, annotations.`32`]
  implicit val traverseTInt32: Traverse[TInt32] = Ann.traverse[TInt, annotations.`32`]

  type TInt64[A] = Ann[TInt, annotations.`64`, A]
  implicit val eqTInt64: Delay[Eq, TInt64]      = Ann.delayEq[TInt, annotations.`64`]
  implicit val traverseTInt64: Traverse[TInt64] = Ann.traverse[TInt, annotations.`64`]

  type TUInt32[A] = Ann[TInt, (annotations.Unsigned, annotations.`32`), A]
  implicit val eqTUInt32: Delay[Eq, TUInt32]      = Ann.delayEq[TInt, (annotations.Unsigned, annotations.`32`)]
  implicit val traverseTUInt32: Traverse[TUInt32] = Ann.traverse[TInt, (annotations.Unsigned, annotations.`32`)]

  type TUInt64[A] = Ann[TInt, (annotations.Unsigned, annotations.`64`), A]
  implicit val eqTUInt64: Delay[Eq, TUInt64]      = Ann.delayEq[TInt, (annotations.Unsigned, annotations.`64`)]
  implicit val traverseTUInt64: Traverse[TUInt64] = Ann.traverse[TInt, (annotations.Unsigned, annotations.`64`)]

  type TSInt32[A] = Ann[TInt, (annotations.Signed, annotations.`32`), A]
  implicit val eqTSInt32: Delay[Eq, TSInt32]      = Ann.delayEq[TInt, (annotations.Signed, annotations.`32`)]
  implicit val traverseTSInt32: Traverse[TSInt32] = Ann.traverse[TInt, (annotations.Signed, annotations.`32`)]

  type TSInt64[A] = Ann[TInt, (annotations.Signed, annotations.`64`), A]
  implicit val eqTSInt64: Delay[Eq, TSInt64]      = Ann.delayEq[TInt, (annotations.Signed, annotations.`64`)]
  implicit val traverseTSInt64: Traverse[TSInt64] = Ann.traverse[TInt, (annotations.Signed, annotations.`64`)]

  type TFixed32[A] = Ann[TInt, (annotations.Fixed, annotations.`32`), A]
  implicit val eqTFixed32: Delay[Eq, TFixed32]      = Ann.delayEq[TInt, (annotations.Fixed, annotations.`32`)]
  implicit val traverseTFixed32: Traverse[TFixed32] = Ann.traverse[TInt, (annotations.Fixed, annotations.`32`)]

  type TFixed64[A] = Ann[TInt, (annotations.Fixed, annotations.`64`), A]
  implicit val eqTFixed64: Delay[Eq, TFixed64]      = Ann.delayEq[TInt, (annotations.Fixed, annotations.`64`)]
  implicit val traverseTFixed64: Traverse[TFixed64] = Ann.traverse[TInt, (annotations.Fixed, annotations.`64`)]

  type TSFixed32[A] = Ann[TInt, (annotations.Fixed, annotations.Signed, annotations.`32`), A]
  implicit val eqTSFixed32: Delay[Eq, TSFixed32] =
    Ann.delayEq[TInt, (annotations.Fixed, annotations.Signed, annotations.`32`)]
  implicit val traverseTSFixed32: Traverse[TSFixed32] =
    Ann.traverse[TInt, (annotations.Fixed, annotations.Signed, annotations.`32`)]

  type TSFixed64[A] = Ann[TInt, (annotations.Fixed, annotations.Signed, annotations.`64`), A]
  implicit val eqTSFixed64: Delay[Eq, TSFixed64] =
    Ann.delayEq[TInt, (annotations.Fixed, annotations.Signed, annotations.`64`)]
  implicit val traverseTSFixed64: Traverse[TSFixed64] =
    Ann.traverse[TInt, (annotations.Fixed, annotations.Signed, annotations.`64`)]

  type TMessage[A] = Ann[TRecord, annotations.Reserved, A]
  implicit val eqTMessage: Delay[Eq, TMessage]      = Ann.delayEq[TRecord, annotations.Reserved]
  implicit val traverseTMessage: Traverse[TMessage] = Ann.traverse[TRecord, annotations.Reserved]

  type Types = TNull :::
    TDouble :::
    TFloat :::
    TInt32 :::
    TInt64 :::
    TUInt32 :::
    TUInt64 :::
    TSInt32 :::
    TSInt64 :::
    TFixed32 :::
    TFixed64 :::
    TSFixed32 :::
    TSFixed64 :::
    TBoolean :::
    TString :::
    TByteArray :::
    TNamedType :::
    TList :::
    TOneOf :::
    TMap :::
    TProtoEnum :::
    TMessage :::
    TFileDescriptor :::
    TNilK

  type Type[A] = CopK[Types, A]

  implicit val InjNull: CopK.Inject[TNull, Type]                     = mkInject[TNull, Types](0)
  implicit val InjDouble: CopK.Inject[TDouble, Type]                 = mkInject[TDouble, Types](1)
  implicit val InjFloat: CopK.Inject[TFloat, Type]                   = mkInject[TFloat, Types](2)
  implicit val InjInt32: CopK.Inject[TInt32, Type]                   = mkInject[TInt32, Types](3)
  implicit val InjInt64: CopK.Inject[TInt64, Type]                   = mkInject[TInt64, Types](4)
  implicit val InjUint32: CopK.Inject[TUInt32, Type]                 = mkInject[TUInt32, Types](5)
  implicit val InjUint64: CopK.Inject[TUInt64, Type]                 = mkInject[TUInt64, Types](6)
  implicit val InjSint32: CopK.Inject[TSInt32, Type]                 = mkInject[TSInt32, Types](7)
  implicit val InjSint64: CopK.Inject[TSInt64, Type]                 = mkInject[TSInt64, Types](8)
  implicit val InjFixed32: CopK.Inject[TFixed32, Type]               = mkInject[TFixed32, Types](9)
  implicit val InjFixed64: CopK.Inject[TFixed64, Type]               = mkInject[TFixed64, Types](10)
  implicit val InjSfixed32: CopK.Inject[TSFixed32, Type]             = mkInject[TSFixed32, Types](11)
  implicit val InjSfixed64: CopK.Inject[TSFixed64, Type]             = mkInject[TSFixed64, Types](12)
  implicit val InjBoolean: CopK.Inject[TBoolean, Type]               = mkInject[TBoolean, Types](13)
  implicit val InjString: CopK.Inject[TString, Type]                 = mkInject[TString, Types](14)
  implicit val InjByteArray: CopK.Inject[TByteArray, Type]           = mkInject[TByteArray, Types](15)
  implicit val InjNamedType: CopK.Inject[TNamedType, Type]           = mkInject[TNamedType, Types](16)
  implicit val InjList: CopK.Inject[TList, Type]                     = mkInject[TList, Types](17)
  implicit val InjOneOf: CopK.Inject[TOneOf, Type]                   = mkInject[TOneOf, Types](18)
  implicit val InjMap: CopK.Inject[TMap, Type]                       = mkInject[TMap, Types](19)
  implicit val InjProtoEnum: CopK.Inject[TProtoEnum, Type]           = mkInject[TProtoEnum, Types](20)
  implicit val InjMessage: CopK.Inject[TMessage, Type]               = mkInject[TMessage, Types](21)
  implicit val InjFileDescriptor: CopK.Inject[TFileDescriptor, Type] = mkInject[TFileDescriptor, Types](22)
  implicit val protoTraverse: Traverse[protobuf.Type]                = derivation.copkTraverse[Types]
  implicit val protoEq: Delay[Eq, protobuf.Type]                     = derivation.delayEqCopK[Types]

  def message[F[α] <: ACopK[α], A](name: String, fields: List[FieldF[A]], reserved: annotations.Reserved)(
      implicit I: TMessage :<<: F) = I.inj(Ann(TRecord(name, fields), reserved))
  def protoEnum[F[α] <: ACopK[α], A](
      name: String,
      symbols: List[(String, Int)],
      options: List[(String, String)],
      aliases: List[(String, Int)])(implicit I: TProtoEnum :<<: F): F[A] =
    I.inj(Ann(TEnum(name, symbols.map(_._1)), annotations.EnumAnnotation(symbols.map(_._2), options, aliases)))
  def int32[F[α] <: ACopK[α], A](implicit I: TInt32 :<<: F) = I.inj(Ann(TInt[A](), annotations.`32`()))
  def int64[F[α] <: ACopK[α], A](implicit I: TInt64 :<<: F) = I.inj(Ann(TInt[A](), annotations.`64`()))
  def sint32[F[α] <: ACopK[α], A](implicit I: TSInt32 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Signed(), annotations.`32`())))
  def sint64[F[α] <: ACopK[α], A](implicit I: TSInt64 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Signed(), annotations.`64`())))
  def uint32[F[α] <: ACopK[α], A](implicit I: TUInt32 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Unsigned(), annotations.`32`())))
  def uint64[F[α] <: ACopK[α], A](implicit I: TUInt64 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Unsigned(), annotations.`64`())))
  def fixed32[F[α] <: ACopK[α], A](implicit I: TFixed32 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Fixed(), annotations.`32`())))
  def fixed64[F[α] <: ACopK[α], A](implicit I: TFixed64 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Fixed(), annotations.`64`())))
  def sfixed32[F[α] <: ACopK[α], A](implicit I: TSFixed32 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Fixed(), annotations.Signed(), annotations.`32`())))
  def sfixed64[F[α] <: ACopK[α], A](implicit I: TSFixed64 :<<: F) =
    I.inj(Ann(TInt[A](), (annotations.Fixed(), annotations.Signed(), annotations.`64`())))
}
