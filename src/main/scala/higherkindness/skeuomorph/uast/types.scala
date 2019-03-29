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

package higherkindness.skeuomorph.uast

import cats.{Eq, Traverse}
import cats.data._
import cats.instances.list._
import monocle.Lens

import higherkindness.skeuomorph.compdata.Ann
import higherkindness.skeuomorph.protobuf.OptionValue
import higherkindness.skeuomorph.avro.types._

import qq.droste.macros.deriveTraverse

import iota.{CopK, TNilK}
import iota.TListK.:::

object types {

  @deriveTraverse final case class Field[A](name: String, tpe: A)
  object Field {
    def tpe[A]: Lens[Field[A], A]       = Lens[Field[A], A](_.tpe)(tpe => field => field.copy(tpe = tpe))
    def name[A]: Lens[Field[A], String] = Lens[Field[A], String](_.name)(name => field => field.copy(name = name))

    implicit val eqField: Delay[Eq, Field] = new Delay[Eq, Field] {
      def apply[A](eq: Eq[A]): Eq[Field[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TNull[A]()
  object TNull {
    implicit val eqTNull: Delay[Eq, TNull] = new Delay[Eq, TNull] {
      def apply[A](eq: Eq[A]): Eq[TNull[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TBoolean[A]()
  object TBoolean {
    implicit val eqTBoolean: Delay[Eq, TBoolean] = new Delay[Eq, TBoolean] {
      def apply[A](eq: Eq[A]): Eq[TBoolean[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TInt[A]()
  object TInt {
    implicit val eqTInt: Delay[Eq, TInt] = new Delay[Eq, TInt] {
      def apply[A](eq: Eq[A]): Eq[TInt[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TLong[A]()
  object TLong {
    implicit val eqTLong: Delay[Eq, TLong] = new Delay[Eq, TLong] {
      def apply[A](eq: Eq[A]): Eq[TLong[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TFloat[A]()
  object TFloat {
    implicit val eqTFloat: Delay[Eq, TFloat] = new Delay[Eq, TFloat] {
      def apply[A](eq: Eq[A]): Eq[TFloat[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TDouble[A]()
  object TDouble {
    implicit val eqTDouble: Delay[Eq, TDouble] = new Delay[Eq, TDouble] {
      def apply[A](eq: Eq[A]): Eq[TDouble[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TString[A]()
  object TString {
    implicit val eqTString: Delay[Eq, TString] = new Delay[Eq, TString] {
      def apply[A](eq: Eq[A]): Eq[TString[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TDate[A]()
  object TDate {
    implicit val eqTDate: Delay[Eq, TDate] = new Delay[Eq, TDate] {
      def apply[A](eq: Eq[A]): Eq[TDate[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TDateTime[A]()
  object TDateTime {
    implicit val eqTDateTime: Delay[Eq, TDateTime] = new Delay[Eq, TDateTime] {
      def apply[A](eq: Eq[A]): Eq[TDateTime[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TPassword[A]()
  object TPassword {
    implicit val eqTPassword: Delay[Eq, TPassword] = new Delay[Eq, TPassword] {
      def apply[A](eq: Eq[A]): Eq[TPassword[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TByte[A]()
  object TByte {
    implicit val eqTByte: Delay[Eq, TByte] = new Delay[Eq, TByte] {
      def apply[A](eq: Eq[A]): Eq[TByte[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TByteArray[A]()
  object TByteArray {
    implicit val eqTByteArray: Delay[Eq, TByteArray] = new Delay[Eq, TByteArray] {
      def apply[A](eq: Eq[A]): Eq[TByteArray[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TNamedType[A](name: String)
  object TNamedType {
    implicit val eqTNamedType: Delay[Eq, TNamedType] = new Delay[Eq, TNamedType] {
      def apply[A](eq: Eq[A]): Eq[TNamedType[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TMap[A](keys: A, values: A)
  object TMap {
    implicit val eqTMap: Delay[Eq, TMap] = new Delay[Eq, TMap] {
      def apply[A](eq: Eq[A]): Eq[TMap[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TNamedFixed[A](name: String, size: Int)
  object TNamedFixed {
    implicit val eqTNamedFixed: Delay[Eq, TNamedFixed] = new Delay[Eq, TNamedFixed] {
      def apply[A](eq: Eq[A]): Eq[TNamedFixed[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TOption[A](value: A)
  object TOption {
    implicit val eqTOption: Delay[Eq, TOption] = new Delay[Eq, TOption] {
      def apply[A](eq: Eq[A]): Eq[TOption[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TEither[A](left: A, right: A)
  object TEither {
    implicit val eqTEither: Delay[Eq, TEither] = new Delay[Eq, TEither] {
      def apply[A](eq: Eq[A]): Eq[TEither[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TList[A](value: A)
  object TList {
    implicit val eqTList: Delay[Eq, TList] = new Delay[Eq, TList] {
      def apply[A](eq: Eq[A]): Eq[TList[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TGeneric[A](generic: A, params: List[A])
  object TGeneric {
    implicit val eqTGeneric: Delay[Eq, TGeneric] = new Delay[Eq, TGeneric] {
      def apply[A](eq: Eq[A]): Eq[TGeneric[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TRecord[A](name: String, fields: List[FieldF[A]])
  object TRecord {
    implicit val eqTRecord: Delay[Eq, TRecord] = new Delay[Eq, TRecord] {
      def apply[A](eq: Eq[A]): Eq[TRecord[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TEnum[A](name: String, symbols: List[String])
  object TEnum {
    implicit val eqTEnum: Delay[Eq, TEnum] = new Delay[Eq, TEnum] {
      def apply[A](eq: Eq[A]): Eq[TEnum[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TUnion[A](options: NonEmptyList[A])
  object TUnion {
    implicit val eqTUnion: Delay[Eq, TUnion] = new Delay[Eq, TUnion] {
      def apply[A](eq: Eq[A]): Eq[TUnion[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TContaining[A](types: List[A])
  object TContaining {
    implicit val eqTContaining: Delay[Eq, TContaining] = new Delay[Eq, TContaining] {
      def apply[A](eq: Eq[A]): Eq[TContaining[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TFileDescriptor[A](values: List[A], name: String, `package`: String)
  object TFileDescriptor {
    implicit val eqTFileDescriptor: Delay[Eq, TFileDescriptor] = new Delay[Eq, TFileDescriptor] {
      def apply[A](eq: Eq[A]): Eq[TFileDescriptor[A]] = Eq.fromUniversalEquals
    }
  }
  @deriveTraverse final case class TOneOf[A](name: String, fields: NonEmptyList[FieldF[A]])
  object TOneOf {
    implicit val eqTOneOf: Delay[Eq, TOneOf] = new Delay[Eq, TOneOf] {
      def apply[A](eq: Eq[A]): Eq[TOneOf[A]] = Eq.fromUniversalEquals
    }
  }

  def `null`[F[α] <: ACopK[α], A](implicit I: TNull :<<: F): F[A]         = I.inj(TNull[A]())
  def boolean[F[α] <: ACopK[α], A](implicit I: TBoolean :<<: F): F[A]     = I.inj(TBoolean[A]())
  def int[F[α] <: ACopK[α], A](implicit I: TInt :<<: F): F[A]             = I.inj(TInt[A]())
  def long[F[α] <: ACopK[α], A](implicit I: TLong :<<: F): F[A]           = I.inj(TLong[A]())
  def float[F[α] <: ACopK[α], A](implicit I: TFloat :<<: F): F[A]         = I.inj(TFloat[A]())
  def double[F[α] <: ACopK[α], A](implicit I: TDouble :<<: F): F[A]       = I.inj(TDouble[A]())
  def string[F[α] <: ACopK[α], A](implicit I: TString :<<: F): F[A]       = I.inj(TString[A]())
  def date[F[α] <: ACopK[α], A](implicit I: TDate :<<: F): F[A]           = I.inj(TDate[A]())
  def dateTime[F[α] <: ACopK[α], A](implicit I: TDateTime :<<: F): F[A]   = I.inj(TDateTime[A]())
  def password[F[α] <: ACopK[α], A](implicit I: TPassword :<<: F): F[A]   = I.inj(TPassword[A]())
  def byte[F[α] <: ACopK[α], A](implicit I: TByte :<<: F): F[A]           = I.inj(TByte[A]())
  def byteArray[F[α] <: ACopK[α], A](implicit I: TByteArray :<<: F): F[A] = I.inj(TByteArray[A]())
  def namedType[F[α] <: ACopK[α], A](name: String)(implicit I: TNamedType :<<: F): F[A] =
    I.inj(TNamedType[A](name))
  def map[F[α] <: ACopK[α], A](keys: A, values: A)(implicit I: TMap :<<: F): F[A] =
    I.inj(TMap[A](keys, values))
  def record[F[α] <: ACopK[α], A](name: String, fields: List[FieldF[A]])(implicit I: TRecord :<<: F): F[A] =
    I.inj(TRecord[A](name, fields))
  def enum[F[α] <: ACopK[α], A](name: String, symbols: List[String])(implicit I: TEnum :<<: F): F[A] =
    I.inj(TEnum[A](name, symbols))
  def union[F[α] <: ACopK[α], A](options: NonEmptyList[A])(implicit I: TUnion :<<: F): F[A] =
    I.inj(TUnion[A](options))
  def fixed[F[α] <: ACopK[α], A](name: String, size: Int)(implicit I: TNamedFixed :<<: F): F[A] =
    I.inj(TNamedFixed[A](name, size))
  def option[F[α] <: ACopK[α], A](value: A)(implicit I: TOption :<<: F): F[A] = I.inj(TOption[A](value))
  def either[F[α] <: ACopK[α], A](left: A, right: A)(implicit I: TEither :<<: F): F[A] =
    I.inj(TEither[A](left, right))
  def list[F[α] <: ACopK[α], A](value: A)(implicit I: TList :<<: F): F[A] = I.inj(TList[A](value))
  def generic[F[α] <: ACopK[α], A](generic: A, params: List[A])(implicit I: TGeneric :<<: F): F[A] =
    I.inj(TGeneric[A](generic, params))
  def containing[F[α] <: ACopK[α], A](types: List[A])(implicit I: TContaining :<<: F): F[A] =
    I.inj(TContaining[A](types))
  def fileDescriptor[F[α] <: ACopK[α], A](values: List[A], name: String, `package`: String)(
      implicit I: TFileDescriptor :<<: F): F[A] =
    I.inj(TFileDescriptor[A](values, name, `package`))
  def oneOf[F[α] <: ACopK[α], A](name: String, fields: NonEmptyList[FieldF[A]])(implicit I: TOneOf :<<: F): F[A] =
    I.inj(TOneOf(name, fields))

  type FieldFTypes = Ann[Field, (Int, List[OptionValue], Boolean, Boolean), ?] :::
    Ann[Field, AvroMetadata, ?] :::
    Field :::
    TNilK

  type FieldF[A] = CopK[
    FieldFTypes,
    A
  ]

  implicit val protoField: Traverse[Ann[Field, (Int, List[OptionValue], Boolean, Boolean), ?]] =
    Ann.traverse[Field, (Int, List[OptionValue], Boolean, Boolean)]
  implicit val avroField: Traverse[Ann[Field, AvroMetadata, ?]] = Ann.traverse[Field, AvroMetadata]
  implicit val fieldFTraverse: Traverse[FieldF]                 = derivation.copkTraverse[FieldF[Unit]#L]

  object FieldF {
    val InjProtobufField = mkInject[Ann[Field, (Int, List[OptionValue], Boolean, Boolean), ?], FieldFTypes](0)
    val InjAvroField     = mkInject[Ann[Field, AvroMetadata, ?], FieldFTypes](1)
    val InjSimpleField   = mkInject[Field, FieldFTypes](2)

    def ProtobufField[A](
        name: String,
        tpe: A,
        position: Int,
        options: List[OptionValue],
        isRepeated: Boolean,
        isMapField: Boolean): FieldF[A] =
      InjProtobufField.inj(Ann(Field[A](name, tpe), (position, options, isRepeated, isMapField)))

    def AvroField[A](
        name: String,
        aliases: List[String],
        doc: Option[String],
        order: Option[Order],
        tpe: A): FieldF[A] =
      InjAvroField.inj(
        Ann(
          Field[A](name, tpe),
          AvroMetadata.AMList(List(AvroMetadata.Aliases(aliases), AvroMetadata.Doc(doc), AvroMetadata.AMOrder(order)))))

    def SimpleField[A](name: String, tpe: A): FieldF[A] = InjSimpleField.inj(Field(name, tpe))

    def field[A]: Lens[FieldF[A], Field[A]] =
      Lens[FieldF[A], Field[A]] {
        case InjProtobufField(Ann(f, _)) => f
        case InjAvroField(Ann(f, _))     => f
        case InjSimpleField(f)           => f
      } { f => ff =>
        ff match {
          case InjProtobufField(Ann(_, ann)) => InjProtobufField.inj(Ann(f, ann))
          case InjAvroField(Ann(_, ann))     => InjAvroField.inj(Ann(f, ann))
          case InjSimpleField(_)             => InjSimpleField.inj(f)
        }
      }

    def fieldName[A]: Lens[FieldF[A], String] = field[A] ^|-> Field.name[A]
    def fieldType[A]: Lens[FieldF[A], A]      = field[A] ^|-> Field.tpe[A]
  }

  // def desugarList[F[α] <: ACopK[α], A](
  //     implicit
  //     L: TList :<<: F,
  //     G: TGeneric :<<: F,
  //     N: TNamedType :<<: F,
  //     A: Embed[F, A]
  // ): Trans[F, F, A] =
  //   Trans {
  //     case L(TList(t)) => generic[F, A](namedType[F, A]("List").embed, List(t))
  //     case x           => x
  //   }

  // def desugarOption[F[α] <: ACopK[α], A](
  //     implicit
  //     O: TOption :<<: F,
  //     G: TGeneric :<<: F,
  //     N: TNamedType :<<: F,
  //     A: Embed[F, A]
  // ): Trans[F, F, A] =
  //   Trans {
  //     case O(TOption(a)) => generic[F, A](namedType[F, A]("Option").embed, List(a))
  //     case x             => x
  //   }

  // def desugarEither[F[α] <: ACopK[α], A](
  //     implicit
  //     E: TEither :<<: F,
  //     G: TGeneric :<<: F,
  //     N: TNamedType :<<: F,
  //     A: Embed[F, A]
  // ): Trans[F, F, A] =
  //   Trans {
  //     case E(TEither(a, b)) => generic[F, A](namedType[F, A]("Either").embed, List(a, b))
  //     case x                => x
  //   }

}
