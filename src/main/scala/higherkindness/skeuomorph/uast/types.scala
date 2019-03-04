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

import cats.data._
import cats.instances.list._
import iota._
import qq.droste.macros.deriveTraverse

object types {

  @deriveTraverse final case class Field[A](name: String, tpe: A)

  @deriveTraverse final case class TNull[A]()
  @deriveTraverse final case class TBoolean[A]()
  @deriveTraverse final case class TInt[A]()
  @deriveTraverse final case class TLong[A]()
  @deriveTraverse final case class TFloat[A]()
  @deriveTraverse final case class TDouble[A]()
  @deriveTraverse final case class TString[A]()
  @deriveTraverse final case class TDate[A]()
  @deriveTraverse final case class TDateTime[A]()
  @deriveTraverse final case class TPassword[A]()
  @deriveTraverse final case class TByte[A]()
  @deriveTraverse final case class TByteArray[A]()
  @deriveTraverse final case class TNamedType[A](name: String)
  @deriveTraverse final case class TMap[A](keys: A, values: A) // sugar over Generic(NamedType("Map"), A, A)
  @deriveTraverse final case class TNamedFixed[A](name: String, size: Int)
  @deriveTraverse final case class TOption[A](value: A)          // sugar over Generic(NamedType("Option"), A)
  @deriveTraverse final case class TEither[A](left: A, right: A) // sugar over Generic(NamedType("Option"), A)
  @deriveTraverse final case class TList[A](value: A)            // sugar over Generic(NamedType("Option"), A)
  @deriveTraverse final case class TGeneric[A](generic: A, params: List[A])
  @deriveTraverse final case class TRecord[A](name: String, fields: List[Field[A]])
  @deriveTraverse final case class TEnum[A](name: String, symbols: List[String])
  @deriveTraverse final case class TUnion[A](options: NonEmptyList[A])
  @deriveTraverse final case class TFileDescriptor[A](values: List[A], name: String, `package`: String)

  def `null`[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TNull, F]): F[A]         = I.inj(TNull[A]())
  def boolean[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TBoolean, F]): F[A]     = I.inj(TBoolean[A]())
  def int[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TInt, F]): F[A]             = I.inj(TInt[A]())
  def long[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TLong, F]): F[A]           = I.inj(TLong[A]())
  def float[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TFloat, F]): F[A]         = I.inj(TFloat[A]())
  def double[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TDouble, F]): F[A]       = I.inj(TDouble[A]())
  def string[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TString, F]): F[A]       = I.inj(TString[A]())
  def date[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TDate, F]): F[A]           = I.inj(TDate[A]())
  def dateTime[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TDateTime, F]): F[A]   = I.inj(TDateTime[A]())
  def password[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TPassword, F]): F[A]   = I.inj(TPassword[A]())
  def byte[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TByte, F]): F[A]           = I.inj(TByte[A]())
  def byteArray[F[α] <: CopK[_, α], A](implicit I: CopK.Inject[TByteArray, F]): F[A] = I.inj(TByteArray[A]())
  def namedType[F[α] <: CopK[_, α], A](name: String)(implicit I: CopK.Inject[TNamedType, F]): F[A] =
    I.inj(TNamedType[A](name))
  def map[F[α] <: CopK[_, α], A](keys: A, values: A)(implicit I: CopK.Inject[TMap, F]): F[A] =
    I.inj(TMap[A](keys, values))
  def record[F[α] <: CopK[_, α], A](name: String, fields: List[Field[A]])(implicit I: CopK.Inject[TRecord, F]): F[A] =
    I.inj(TRecord[A](name, fields))
  def enum[F[α] <: CopK[_, α], A](name: String, symbols: List[String])(implicit I: CopK.Inject[TEnum, F]): F[A] =
    I.inj(TEnum[A](name, symbols))
  def union[F[α] <: CopK[_, α], A](options: NonEmptyList[A])(implicit I: CopK.Inject[TUnion, F]): F[A] =
    I.inj(TUnion[A](options))
  def fixed[F[α] <: CopK[_, α], A](name: String, size: Int)(implicit I: CopK.Inject[TNamedFixed, F]): F[A] =
    I.inj(TNamedFixed[A](name, size))
  def option[F[α] <: CopK[_, α], A](value: A)(implicit I: CopK.Inject[TOption, F]): F[A] = I.inj(TOption[A](value))
  def either[F[α] <: CopK[_, α], A](left: A, right: A)(implicit I: CopK.Inject[TEither, F]): F[A] =
    I.inj(TEither[A](left, right))
  def list[F[α] <: CopK[_, α], A](value: A)(implicit I: CopK.Inject[TList, F]): F[A] = I.inj(TList[A](value))
  def generic[F[α] <: CopK[_, α], A](generic: A, params: List[A])(implicit I: CopK.Inject[TGeneric, F]): F[A] =
    I.inj(TGeneric[A](generic, params))
  def fileDescriptor[F[α] <: CopK[_, α], A](values: List[A], name: String, `package`: String)(
      implicit I: CopK.Inject[TFileDescriptor, F]): F[A] =
    I.inj(TFileDescriptor[A](values, name, `package`))

  // def desugarList[F[α] <: CopK[_, α], A](
  //     implicit
  //     L: CopK.Inject[TList, F],
  //     G: CopK.Inject[TGeneric, F],
  //     N: CopK.Inject[TNamedType, F],
  //     A: Embed[F, A]
  // ): Trans[F, F, A] =
  //   Trans {
  //     case L(TList(t)) => generic[F, A](namedType[F, A]("List").embed, List(t))
  //     case x           => x
  //   }

  // def desugarOption[F[α] <: CopK[_, α], A](
  //     implicit
  //     O: CopK.Inject[TOption, F],
  //     G: CopK.Inject[TGeneric, F],
  //     N: CopK.Inject[TNamedType, F],
  //     A: Embed[F, A]
  // ): Trans[F, F, A] =
  //   Trans {
  //     case O(TOption(a)) => generic[F, A](namedType[F, A]("Option").embed, List(a))
  //     case x             => x
  //   }

  // def desugarEither[F[α] <: CopK[_, α], A](
  //     implicit
  //     E: CopK.Inject[TEither, F],
  //     G: CopK.Inject[TGeneric, F],
  //     N: CopK.Inject[TNamedType, F],
  //     A: Embed[F, A]
  // ): Trans[F, F, A] =
  //   Trans {
  //     case E(TEither(a, b)) => generic[F, A](namedType[F, A]("Either").embed, List(a, b))
  //     case x                => x
  //   }

}
