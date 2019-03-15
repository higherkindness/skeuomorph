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

import higherkindness.skeuomorph.uast.{:<<:, ACopK}
import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.compdata.Ann

package object protobuf {

  // annotations used for adding extra data to uast types
  object annotations {
    final case class `32`()
    final case class `64`()
    final case class Signed()
    final case class Unsigned()
    final case class Fixed()
    final case class Reserved(reserved: List[List[String]])
    final case class EnumAnnotation(fieldNumbers: List[Int], options: List[OptionValue], aliases: List[(String, Int)])
  }

  type TProtoEnum[A] = Ann[TEnum, annotations.EnumAnnotation, A]
  type TInt32[A]     = Ann[TInt, annotations.`32`, A]
  type TInt64[A]     = Ann[TInt, annotations.`64`, A]
  type TUInt32[A]    = Ann[TInt, (annotations.Unsigned, annotations.`32`), A]
  type TUInt64[A]    = Ann[TInt, (annotations.Unsigned, annotations.`64`), A]
  type TSInt32[A]    = Ann[TInt, (annotations.Signed, annotations.`32`), A]
  type TSInt64[A]    = Ann[TInt, (annotations.Signed, annotations.`64`), A]
  type TFixed32[A]   = Ann[TInt, (annotations.Fixed, annotations.`32`), A]
  type TFixed64[A]   = Ann[TInt, (annotations.Fixed, annotations.`64`), A]
  type TSFixed32[A]  = Ann[TInt, (annotations.Fixed, annotations.Signed, annotations.`32`), A]
  type TSFixed64[A]  = Ann[TInt, (annotations.Fixed, annotations.Signed, annotations.`64`), A]
  type TMessage[A]   = Ann[TRecord, annotations.Reserved, A]

  type Type[A] = CopK[
    TNull :::
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
      TNilK,
    A
  ]

  val InjNull: CopK.Inject[TNull, Type]                     = CopK.Inject[TNull, Type]
  val InjDouble: CopK.Inject[TDouble, Type]                 = CopK.Inject[TDouble, Type]
  val InjFloat: CopK.Inject[TFloat, Type]                   = CopK.Inject[TFloat, Type]
  val InjInt32: CopK.Inject[TInt32, Type]                   = CopK.Inject[TInt32, Type]
  val InjInt64: CopK.Inject[TInt64, Type]                   = CopK.Inject[TInt64, Type]
  val InjUint32: CopK.Inject[TUInt32, Type]                 = CopK.Inject[TUInt32, Type]
  val InjUint64: CopK.Inject[TUInt64, Type]                 = CopK.Inject[TUInt64, Type]
  val InjSint32: CopK.Inject[TSInt32, Type]                 = CopK.Inject[TSInt32, Type]
  val InjSint64: CopK.Inject[TSInt64, Type]                 = CopK.Inject[TSInt64, Type]
  val InjFixed32: CopK.Inject[TFixed32, Type]               = CopK.Inject[TFixed32, Type]
  val InjFixed64: CopK.Inject[TFixed64, Type]               = CopK.Inject[TFixed64, Type]
  val InjSfixed32: CopK.Inject[TSFixed32, Type]             = CopK.Inject[TSFixed32, Type]
  val InjSfixed64: CopK.Inject[TSFixed64, Type]             = CopK.Inject[TSFixed64, Type]
  val InjBoolean: CopK.Inject[TBoolean, Type]               = CopK.Inject[TBoolean, Type]
  val InjString: CopK.Inject[TString, Type]                 = CopK.Inject[TString, Type]
  val InjByteArray: CopK.Inject[TByteArray, Type]           = CopK.Inject[TByteArray, Type]
  val InjNamedType: CopK.Inject[TNamedType, Type]           = CopK.Inject[TNamedType, Type]
  val InjList: CopK.Inject[TList, Type]                     = CopK.Inject[TList, Type]
  val InjOneOf: CopK.Inject[TOneOf, Type]                   = CopK.Inject[TOneOf, Type]
  val InjMap: CopK.Inject[TMap, Type]                       = CopK.Inject[TMap, Type]
  val InjProtoEnum: CopK.Inject[TProtoEnum, Type]           = CopK.Inject[TProtoEnum, Type]
  val InjMessage: CopK.Inject[TMessage, Type]               = CopK.Inject[TMessage, Type]
  val InjFileDescriptor: CopK.Inject[TFileDescriptor, Type] = CopK.Inject[TFileDescriptor, Type]

  def message[F[α] <: ACopK[α], A](name: String, fields: List[FieldF[A]], reserved: annotations.Reserved)(
      implicit I: TMessage :<<: F) = I.inj(Ann(TRecord(name, fields), reserved))
  def protoEnum[F[α] <: ACopK[α], A](
      name: String,
      symbols: List[(String, Int)],
      options: List[OptionValue],
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
