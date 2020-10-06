/*
 * Copyright 2018-2020 47 Degrees Open Source <https://www.47deg.com>
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

package higherkindness.skeuomorph.mu

import higherkindness.droste.macros.deriveTraverse
import higherkindness.droste.{Algebra, Project}
import higherkindness.droste.scheme.cata
import higherkindness.skeuomorph.{protobuf => pb}
import cats.Eq
import cats.Show
import cats.data.NonEmptyList
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.instances.option._
import cats.syntax.eq._

@deriveTraverse sealed trait MuF[A]
object MuF {
  @deriveTraverse final case class Field[A](name: String, tpe: A, indices: Option[List[Int]])

  final case class SumField(name: String, value: Int)

  sealed trait Length
  case object Length {
    case class Fixed(length: Int) extends Length
    case object Arbitrary extends Length
  }



  sealed trait NumberSize extends Product with Serializable
  case object _32         extends NumberSize
  case object _64         extends NumberSize

  sealed abstract class TInt[A](val size: NumberSize) extends MuF[A]
  object TInt {
    def unapply[A](x: TInt[A]): Option[NumberSize] = Some(x.size)
  }
  final case class TSimpleInt[A](override val size: NumberSize) extends TInt[A](size) with MuF[A]
  final case class TProtobufInt[A](override val size: NumberSize, modifiers: List[pb.IntModifier])
      extends TInt[A](size)
      with MuF[A]

  final case class TNull[A]()                                        extends MuF[A]
  final case class TDouble[A]()                                      extends MuF[A]
  final case class TFloat[A]()                                       extends MuF[A]
  final case class TBoolean[A]()                                     extends MuF[A]
  final case class TString[A]()                                      extends MuF[A]
  final case class TByteArray[A](length: Length)                     extends MuF[A]
  final case class TNamedType[A](prefix: List[String], name: String) extends MuF[A]
  final case class TOption[A](value: A)                              extends MuF[A]
  final case class TEither[A](left: A, right: A)                     extends MuF[A]
  final case class TList[A](value: A)                                extends MuF[A]
  final case class TMap[A](keyTpe: Option[A], value: A)              extends MuF[A]
  final case class TGeneric[A](generic: A, params: List[A])          extends MuF[A]
  final case class TContaining[A](values: List[A])                   extends MuF[A]
  final case class TRequired[A](value: A)                            extends MuF[A]
  final case class TCoproduct[A](invariants: NonEmptyList[A])        extends MuF[A]
  final case class TSum[A](name: String, fields: List[SumField])     extends MuF[A]
  final case class TProduct[A](
                                name: String,
                                namespace: Option[String],
                                fields: List[Field[A]],
                                nestedProducts: List[A],
                                nestedCoproducts: List[A]
  )                                                        extends MuF[A]
  final case class TDate[A]()                              extends MuF[A]
  final case class TInstant[A]()                           extends MuF[A]
  final case class TUUID[A]()                              extends MuF[A]
  final case class TDecimal[A](precision: Int, scale: Int) extends MuF[A]

  implicit def fieldEq[T](implicit T: Eq[T]): Eq[Field[T]] =
    Eq.instance { case (Field(n, t, is), Field(n2, t2, is2)) =>
      n === n2 && t === t2 && is === is2
    }

  implicit val sumFieldEq: Eq[SumField] = Eq.instance { case (SumField(n, v), SumField(n2, v2)) =>
    n === n2 && v === v2
  }

  implicit val lengthEq: Eq[Length] = Eq.instance {
    case (Length.Arbitrary, Length.Arbitrary) => true
    case (Length.Fixed(l), Length.Fixed(l2)) => l === l2
    case _ => false
  }

  implicit val numberSizeEq: Eq[NumberSize] = Eq.fromUniversalEquals

  implicit val pbIntModifierEq: Eq[pb.IntModifier] = Eq.fromUniversalEquals

  implicit def muEq[T](implicit T: Eq[T]): Eq[MuF[T]] =
    Eq.instance {
      case (TNull(), TNull())                     => true
      case (TDouble(), TDouble())                 => true
      case (TFloat(), TFloat())                   => true
      case (TSimpleInt(size1), TSimpleInt(size2)) => size1 === size2
      case (TProtobufInt(size1, mods1), TProtobufInt(size2, mods2)) =>
        size1 === size2 && mods1.sortBy(_.toString) === mods2.sortBy(_.toString)
      case (TBoolean(), TBoolean())     => true
      case (TString(), TString())       => true
      case (TByteArray(l), TByteArray(l2)) => l === l2

      case (TNamedType(p1, a), TNamedType(p2, b)) => p1 === p2 && a === b
      case (TOption(a), TOption(b))               => a === b
      case (TList(a), TList(b))                   => a === b
      case (TMap(k1, a), TMap(k2, b))             => k1 === k2 && a === b
      case (TRequired(a), TRequired(b))           => a === b

      case (TContaining(a), TContaining(b))                     => a === b
      case (TEither(l, r), TEither(l2, r2))                     => l === l2 && r === r2
      case (TGeneric(g, p), TGeneric(g2, p2))                   => g === g2 && p === p2
      case (TCoproduct(i), TCoproduct(i2))                      => i === i2
      case (TSum(n, f), TSum(n2, f2))                           => n === n2 && f === f2
      case (TProduct(n, ns, f, np, nc), TProduct(n2, ns2, f2, np2, nc2)) => n === n2 && ns === ns2 && f === f2 && np === np2 && nc === nc2

      case _ => false
    }

  implicit def muShow[T](implicit T: Project[MuF, T]): Show[T] =
    Show.show {
      cata(Algebra[MuF, String] {
        case TNull()          => "null"
        case TDouble()        => "double"
        case TFloat()         => "float"
        case TInt(`_32`)      => "int"
        case TInt(`_64`)      => "long"
        case TBoolean()       => "boolean"
        case TString()        => "string"
        case TByteArray(length)     => length match {
          case Length.Fixed(n) => s"bytes[${n}]"
          case Length.Arbitrary => "bytes"
        }
        case TNamedType(p, n) => if (p.isEmpty) n else s"${p.mkString(".")}.$n"
        case TOption(v)       => s"?$v"
        case TList(e)         => s"[$e]"
        case TMap(k, v)       => s"$k->$v"
        case TRequired(v)     => v
        case TContaining(ts)  => ts.mkString("cont<", ", ", ">")
        case TEither(l, r)    => s"either<$l, $r>"
        case TGeneric(g, ps)  => ps.mkString(s"$g<", ", ", ">")
        case TCoproduct(ts)   => ts.toList.mkString("(", " | ", ")")
        case TSum(n, vs)      => vs.map(_.name).mkString(s"$n[", ", ", "]")
        case TProduct(n, _, fields, _, _) =>
          fields.map(f => s"@pbIndex(${f.indices.mkString(",")}) ${f.name}: ${f.tpe}").mkString(s"$n{", ", ", "}")

      })
    }

  // smart constructors, to avoid scala inferring specific types instead of MuF
  def `null`[A](): MuF[A]                                      = TNull()
  def double[A](): MuF[A]                                      = TDouble()
  def float[A](): MuF[A]                                       = TFloat()
  def int[A](): MuF[A]                                         = TSimpleInt(_32)
  def pbInt[A](mods: pb.IntModifier*): MuF[A]                  = TProtobufInt(_32, mods.toList)
  def long[A](): MuF[A]                                        = TSimpleInt(_64)
  def pbLong[A](mods: pb.IntModifier*): MuF[A]                 = TProtobufInt(_64, mods.toList)
  def boolean[A](): MuF[A]                                     = TBoolean()
  def string[A](): MuF[A]                                      = TString()
  def byteArray[A](length: Length): MuF[A]                     = TByteArray(length)
  def namedType[A](prefix: List[String], name: String): MuF[A] = TNamedType(prefix, name)
  def option[A](value: A): MuF[A]                              = TOption(value)
  def either[A](left: A, right: A): MuF[A]                     = TEither(left, right)
  def list[A](value: A): MuF[A]                                = TList(value)
  def map[A](maybeKey: Option[A], value: A): MuF[A]            = TMap(maybeKey, value)
  def generic[A](generic: A, params: List[A]): MuF[A]          = TGeneric(generic, params)
  def required[A](value: A): MuF[A]                            = TRequired(value)
  def coproduct[A](invariants: NonEmptyList[A]): MuF[A]        = TCoproduct(invariants)
  def sum[A](name: String, fields: List[SumField]): MuF[A]     = TSum(name, fields)
  def product[A](name: String, nameSpace: Option[String], fields: List[Field[A]], nestedProducts: List[A], nestedCoproducts: List[A]): MuF[A] =
    TProduct(name, nameSpace, fields, nestedProducts, nestedCoproducts)
}
