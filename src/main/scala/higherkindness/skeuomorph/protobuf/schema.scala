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

package higherkindness.skeuomorph.protobuf

import cats.{Applicative, Eq}
import cats.data.NonEmptyList
import cats.implicits._
import higherkindness.skeuomorph.protobuf.ProtobufF.Option
import qq.droste.Coalgebra
import qq.droste.util.DefaultTraverse

sealed trait FieldF[A] {
  val name: String
  val tpe: A
}

object FieldF {
  final case class Field[A](
      name: String,
      tpe: A,
      position: Int,
      options: List[Option],
      isRepeated: Boolean,
      isMapField: Boolean)
      extends FieldF[A]

  final case class OneOfField[A](name: String, tpe: A) extends FieldF[A]

  implicit def fieldEq[T: Eq]: Eq[FieldF[T]] = Eq.instance {
    case (Field(n, t, p, o, r, m), Field(n2, t2, p2, o2, r2, m2)) =>
      n === n2 && t === t2 && p === p2 && o === o2 && r === r2 && m === m2
    case (OneOfField(n, tpe), OneOfField(n2, tpe2)) =>
      n === n2 && tpe === tpe2
    case _ => false
  }
}

sealed trait ProtobufF[A]

object ProtobufF {

  final case class Option(name: String, value: String)
  object Option {
    implicit val optionEq: Eq[Option] = Eq.instance {
      case (Option(n, v), Option(n2, v2)) => n === n2 && v === v2
    }
  }

  final case class TDouble[A]()                                                   extends ProtobufF[A]
  final case class TFloat[A]()                                                    extends ProtobufF[A]
  final case class TInt32[A]()                                                    extends ProtobufF[A]
  final case class TInt64[A]()                                                    extends ProtobufF[A]
  final case class TUint32[A]()                                                   extends ProtobufF[A]
  final case class TUint64[A]()                                                   extends ProtobufF[A]
  final case class TSint32[A]()                                                   extends ProtobufF[A]
  final case class TSint64[A]()                                                   extends ProtobufF[A]
  final case class TFixed32[A]()                                                  extends ProtobufF[A]
  final case class TFixed64[A]()                                                  extends ProtobufF[A]
  final case class TSfixed32[A]()                                                 extends ProtobufF[A]
  final case class TSfixed64[A]()                                                 extends ProtobufF[A]
  final case class TBool[A]()                                                     extends ProtobufF[A]
  final case class TString[A]()                                                   extends ProtobufF[A]
  final case class TBytes[A]()                                                    extends ProtobufF[A]
  final case class TNamedType[A](name: String)                                    extends ProtobufF[A]
  final case class TRepeated[A](value: A)                                         extends ProtobufF[A]
  final case class TOneOf[A](name: String, fields: NonEmptyList[FieldF.Field[A]]) extends ProtobufF[A]
  final case class TMap[A](keyTpe: A, value: A)                                   extends ProtobufF[A]
  final case class TEnum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[Option],
      aliases: List[(String, Int)])
      extends ProtobufF[A]
  final case class TMessage[A](name: String, fields: List[FieldF[A]], reserved: List[List[String]]) extends ProtobufF[A]
  final case class TFileDescriptor[A](values: List[A], name: String, `package`: String)             extends ProtobufF[A]

  def double[A](): ProtobufF[A]                                                   = TDouble()
  def float[A](): ProtobufF[A]                                                    = TFloat()
  def int32[A](): ProtobufF[A]                                                    = TInt32()
  def int64[A](): ProtobufF[A]                                                    = TInt64()
  def uint32[A](): ProtobufF[A]                                                   = TUint32()
  def uint64[A](): ProtobufF[A]                                                   = TUint64()
  def sint32[A](): ProtobufF[A]                                                   = TSint32()
  def sint64[A](): ProtobufF[A]                                                   = TSint64()
  def fixed32[A](): ProtobufF[A]                                                  = TFixed32()
  def fixed64[A](): ProtobufF[A]                                                  = TFixed64()
  def sfixed32[A](): ProtobufF[A]                                                 = TSfixed32()
  def sfixed64[A](): ProtobufF[A]                                                 = TSfixed64()
  def bool[A](): ProtobufF[A]                                                     = TBool()
  def string[A](): ProtobufF[A]                                                   = TString()
  def bytes[A](): ProtobufF[A]                                                    = TBytes()
  def namedType[A](name: String): ProtobufF[A]                                    = TNamedType(name)
  def repeated[A](value: A): ProtobufF[A]                                         = TRepeated(value)
  def oneOf[A](name: String, fields: NonEmptyList[FieldF.Field[A]]): ProtobufF[A] = TOneOf(name, fields)
  def map[A](keyTpe: A, value: A): ProtobufF[A]                                   = TMap(keyTpe, value)
  def enum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[Option],
      aliases: List[(String, Int)]): ProtobufF[A] = TEnum(name, symbols, options, aliases)
  def message[A](name: String, fields: List[FieldF[A]], reserved: List[List[String]]): ProtobufF[A] =
    TMessage(name, fields, reserved)

  implicit def protobufEq[T: Eq]: Eq[ProtobufF[T]] = Eq.instance {
    case (TDouble(), TDouble())                     => true
    case (TFloat(), TFloat())                       => true
    case (TInt32(), TInt32())                       => true
    case (TInt64(), TInt64())                       => true
    case (TUint32(), TUint32())                     => true
    case (TUint64(), TUint64())                     => true
    case (TSint32(), TSint32())                     => true
    case (TSint64(), TSint64())                     => true
    case (TFixed32(), TFixed32())                   => true
    case (TFixed64(), TFixed64())                   => true
    case (TSfixed32(), TSfixed32())                 => true
    case (TSfixed64(), TSfixed64())                 => true
    case (TBool(), TBool())                         => true
    case (TString(), TString())                     => true
    case (TBytes(), TBytes())                       => true
    case (TNamedType(n), TNamedType(n2))            => n === n2
    case (TRepeated(v), TRepeated(v2))              => v === v2
    case (TEnum(n, s, o, a), TEnum(n2, s2, o2, a2)) => n === n2 && s === s2 && o === o2 && a === a2
    case (TMessage(n, f, r), TMessage(n2, f2, r2))  => n === n2 && f === f2 && r === r2
    case _                                          => false
  }

  implicit val traverse: DefaultTraverse[ProtobufF] = new DefaultTraverse[ProtobufF] {
    def traverse[G[_], A, B](fa: ProtobufF[A])(f: A => G[B])(implicit G: Applicative[G]): G[ProtobufF[B]] = {

      def makeFieldB(field: FieldF.Field[A]): G[FieldF.Field[B]] =
        f(field.tpe).map(b =>
          FieldF.Field[B](field.name, b, field.position, field.options, field.isRepeated, field.isMapField))

      def makeOneOfB(oneOf: FieldF.OneOfField[A]): G[FieldF[B]] =
        f(oneOf.tpe).map(b => FieldF.OneOfField[B](oneOf.name, b): FieldF[B])

      def traverseFieldF(fieldFList: List[FieldF[A]]): G[List[FieldF[B]]] =
        fieldFList.traverse {
          case field: FieldF.Field[A]      => makeFieldB(field).widen
          case oneOf: FieldF.OneOfField[A] => makeOneOfB(oneOf).widen
        }

      fa match {
        case TDouble()                              => double[B]().pure[G]
        case TFloat()                               => float[B]().pure[G]
        case TInt32()                               => int32[B]().pure[G]
        case TInt64()                               => int64[B]().pure[G]
        case TUint32()                              => uint32[B]().pure[G]
        case TUint64()                              => uint64[B]().pure[G]
        case TSint32()                              => sint32[B]().pure[G]
        case TSint64()                              => sint64[B]().pure[G]
        case TFixed32()                             => fixed32[B]().pure[G]
        case TFixed64()                             => fixed64[B]().pure[G]
        case TSfixed32()                            => sfixed32[B]().pure[G]
        case TSfixed64()                            => sfixed64[B]().pure[G]
        case TBool()                                => bool[B]().pure[G]
        case TString()                              => string[B]().pure[G]
        case TBytes()                               => bytes[B]().pure[G]
        case TNamedType(name)                       => namedType[B](name).pure[G]
        case TRepeated(value)                       => f(value).map(TRepeated[B])
        case TOneOf(name, fields)                   => fields.traverse(makeFieldB).map(bFields => TOneOf(name, bFields))
        case TMap(keyTpe, value)                    => (f(keyTpe), f(value)).mapN(TMap[B])
        case TEnum(name, symbols, options, aliases) => enum[B](name, symbols, options, aliases).pure[G]: G[ProtobufF[B]]
        case TMessage(name, fields, reserved) =>
          traverseFieldF(fields).map(bFields => TMessage[B](name, bFields, reserved))
        case TFileDescriptor(values, name, p) =>
          values.traverse(f).map(bValues => TFileDescriptor(bValues, name, p))
      }
    }
  }

  def fromProtobuf: Coalgebra[ProtobufF, NativeDescriptor] = Coalgebra {
    case f: NativeFile      => fileFromDescriptor(f)
    case e: NativeEnum      => enumFromDescriptor(e)
    case o: NativeOneOf     => oneOfFromDescriptor(o)
    case d: NativeMessage   => messageFromDescriptor(d)
    case r: NativeRepeated  => repeatedFromDescriptor(r)
    case m: NativeMap       => mapDescriptor(m)
    case n: NativeNamedType => namedFromDescriptor(n)
    case _: NativeBool      => TBool()
    case _: NativeBytes     => TBytes()
    case _: NativeDouble    => TDouble()
    case _: NativeFixed32   => TFixed32()
    case _: NativeFixed64   => TFixed64()
    case _: NativeFloat     => TFloat()
    case _: NativeInt32     => TInt32()
    case _: NativeInt64     => TInt64()
    case _: NativeSfixed32  => TSfixed32()
    case _: NativeSfixed64  => TSfixed64()
    case _: NativeSint32    => TSint32()
    case _: NativeSint64    => TSint64()
    case _: NativeString    => TString()
    case _: NativeUint32    => TUint32()
    case _: NativeUint64    => TUint64()
  }

  def fileFromDescriptor(fd: NativeFile): TFileDescriptor[NativeDescriptor] =
    TFileDescriptor(fd.values, fd.name, fd.`package`)

  def enumFromDescriptor(e: NativeEnum): TEnum[NativeDescriptor] =
    TEnum(e.name, e.symbols, e.options.map(toTOption), e.aliases)

  def oneOfFromDescriptor(o: NativeOneOf): TOneOf[NativeDescriptor] =
    TOneOf[NativeDescriptor](
      o.name,
      o.fields.map(
        f =>
          FieldF
            .Field[NativeDescriptor](f.name, f.tpe, f.position, f.options.map(toTOption), f.isRepeated, f.isMapField)))

  def messageFromDescriptor(msg: NativeMessage): TMessage[NativeDescriptor] =
    TMessage[NativeDescriptor](msg.name, msg.fields.collect(toFieldF), msg.reserved)

  def repeatedFromDescriptor(r: NativeRepeated): TRepeated[NativeDescriptor] =
    TRepeated[NativeDescriptor](r.value)

  def mapDescriptor(m: NativeMap): TMap[NativeDescriptor] =
    TMap[NativeDescriptor](m.keyTpe, m.value)

  def namedFromDescriptor(n: NativeNamedType): TNamedType[NativeDescriptor] =
    TNamedType[NativeDescriptor](n.name)

  def toFieldF: PartialFunction[NativeFieldF, FieldF[NativeDescriptor]] = {
    case f: NativeField =>
      FieldF.Field[NativeDescriptor](f.name, f.tpe, f.position, f.options.map(toTOption), f.isRepeated, f.isMapField)
    case f: NativeOneOfField =>
      FieldF.OneOfField[NativeDescriptor](f.name, f.tpe)
  }

  def toTOption(no: NativeOption): ProtobufF.Option =
    ProtobufF.Option(no.name, no.value)

}
