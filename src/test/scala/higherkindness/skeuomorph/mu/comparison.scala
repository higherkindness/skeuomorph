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

package higherkindness.skeuomorph.mu

import qq.droste._
import qq.droste.syntax.project._
import MuF._
import cats.Functor
import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.apply._
import cats.syntax.show._

trait PathElement extends Product with Serializable
object PathElement {
  final case class Name(value: String)        extends PathElement
  final case class FieldName(name: String)    extends PathElement
  case object Values                          extends PathElement
  case object Keys                            extends PathElement
  case object Items                           extends PathElement
  final case class Alternative(idx: Int)      extends PathElement
  case object LeftBranch                      extends PathElement
  case object RightBranch                     extends PathElement
  case object GenericType                     extends PathElement
  final case class GenericParameter(idx: Int) extends PathElement

  implicit val pathElementShow: Show[PathElement] = Show.show {
    case Name(v)             => v
    case FieldName(n)        => n
    case Values              => "$values"
    case Keys                => "$keys"
    case Items               => "$items"
    case Alternative(i)      => s"$$alt[$i]"
    case LeftBranch          => "$left"
    case RightBranch         => "$right"
    case GenericType         => "$gtype"
    case GenericParameter(i) => s"$$tparam[$i]"
  }
}

final case class Path(elements: Vector[PathElement]) {
  def /(elem: PathElement) = Path(elements :+ elem)
}
object Path {
  def empty: Path = Path(Vector.empty)

  implicit def pathShow(implicit elem: Show[PathElement]): Show[Path] =
    Show.show(_.elements.map(elem.show).mkString("."))

  def commonAncestor(p1: Path, p2: Path): Path =
    Path(p1.elements.zip(p2.elements).takeWhile(p => p._1 == p._2).map(_._1))
}

sealed trait Transformation[T]

object Transformation {
  final case class NumericWiddening[T](relativePath: Path, from: T, to: T) extends Transformation[T]
  final case class Addition[T](relativePath: Path, added: T)               extends Transformation[T]
  final case class Removal[T](relativePath: Path, removed: T)              extends Transformation[T]

  implicit def transformationShow[T](implicit showT: Show[T]): Show[Transformation[T]] = Show.show {
    case NumericWiddening(p, f, t) => p.show ++ ": numeric widdening from " ++ f.show ++ " to " ++ t.show
    case Addition(p, a)            => p.show ++ ": added field with schema " ++ a.show
    case Removal(p, _)             => p.show ++ ": field removed"
  }
}

sealed trait Incompatibility

object Incompatibility {
  final case class Different(relativePath: Path)          extends Incompatibility
  final case class UnionMemberRemoved(relativePath: Path) extends Incompatibility

  implicit val incompatibilityShow: Show[Incompatibility] = Show.show {
    case Different(p)          => p.show ++ ": !!DIFFERENT!!"
    case UnionMemberRemoved(p) => p.show ++ ": union member not found in reader schema"
  }
}

sealed trait ComparisonResult[T]
object ComparisonResult {

  final case class Match[T](transformations: List[Transformation[T]]) extends ComparisonResult[T]
  final case class Mismatch[T](transformations: List[Transformation[T]], discrepancies: NonEmptyList[Incompatibility])
      extends ComparisonResult[T]

  def isMatch[T](result: ComparisonResult[T]): Boolean = result match {
    case Match(_)       => true
    case Mismatch(_, _) => false
  }

  implicit def comparisonResultCatsMonoid[T]: Monoid[ComparisonResult[T]] =
    new Monoid[ComparisonResult[T]] {
      def empty = Match[T](Nil)
      def combine(left: ComparisonResult[T], right: ComparisonResult[T]): ComparisonResult[T] = (left, right) match {
        case (Match(t1), Match(t2))               => Match(t1 ++ t2)
        case (Match(t1), Mismatch(t2, d2))        => Mismatch(t1 ++ t2, d2)
        case (Mismatch(t1, d1), Match(t2))        => Mismatch(t1 ++ t2, d1)
        case (Mismatch(t1, d1), Mismatch(t2, d2)) => Mismatch(t1 ++ t2, d1 ++ d2.toList)
      }
    }

  implicit def comparisonResultShow[T](
      implicit showTrans: Show[Transformation[T]],
      showIncomp: Show[Incompatibility]): Show[ComparisonResult[T]] = Show.show {
    case Match(Nil)     => "schemas are identical"
    case Match(t)       => "compatible transformations detected:\n" ++ t.map(_.show).mkString("\n")
    case Mismatch(_, i) => "schemas are incompatible:\n" ++ i.toList.map(_.show).mkString("\n")
  }

}

sealed trait Comparison[T, A]
object Comparison {

  import ComparisonResult._
  import PathElement._
  import Transformation._
  import Incompatibility._

  final case class Result[T, A](result: ComparisonResult[T])            extends Comparison[T, A]
  final case class Compare[T, A](a: A)                                  extends Comparison[T, A]
  final case class CompareBoth[T, A](x: A, y: A)                        extends Comparison[T, A]
  final case class CompareList[T, A](items: List[A])                    extends Comparison[T, A]
  final case class MatchInList[T, A](attempts: Vector[A])               extends Comparison[T, A]
  final case class AlignUnionMembers[T, A](attempts: Map[Int, List[A]]) extends Comparison[T, A]

  implicit def comparisonCatsFunctor[T] = new Functor[Comparison[T, ?]] {
    def map[A, B](fa: Comparison[T, A])(f: (A) => B): Comparison[T, B] = fa match {
      case Result(res)          => Result(res)
      case Compare(a)           => Compare(f(a))
      case CompareBoth(x, y)    => CompareBoth(f(x), f(y))
      case CompareList(i)       => CompareList(i.map(f))
      case MatchInList(a)       => MatchInList(a.map(f))
      case AlignUnionMembers(a) => AlignUnionMembers(a.mapValues(_.map(f)))
    }
  }

  type Context[T] = (Path, Option[T], Option[T])

  def zipLists[T](path: Path, l1: List[T], l2: List[T], pathElem: Int => PathElement): List[Context[T]] = {
    val l1s = l1.toStream.map(_.some) ++ Stream.continually(None)
    val l2s = l2.toStream.map(_.some) ++ Stream.continually(None)
    l1s.zip(l2s).takeWhile((None, None) != _).toList.zipWithIndex.map {
      case (p, i) => (path / pathElem(i), p._1, p._2)
    }
  }

  def same[T, A]: Comparison[T, A] = Result(Match(Nil))

  def coalg[T](implicit basis: Basis[MuF, T]): (Context[T]) => Comparison[T, Context[T]] = {
    case (path, w @ Some(writer), Some(reader)) =>
      (writer.project, reader.project) match {
        case (TNull(), TNull())           => same
        case (TDouble(), TDouble())       => same
        case (TFloat(), TFloat())         => same
        case (TInt(), TInt())             => same
        case (TLong(), TLong())           => same
        case (TBoolean(), TBoolean())     => same
        case (TString(), TString())       => same
        case (TByteArray(), TByteArray()) => same

        // Numeric widdening
        case (TInt(), TLong() | TFloat() | TDouble()) | (TLong(), TFloat() | TDouble()) | (TFloat(), TDouble()) =>
          Result(Match(List(NumericWiddening(path, writer, reader))))

        // String and Byte arrays are considered the same
        case (TByteArray(), TString()) | (TString(), TByteArray()) => same

        case (TNamedType(a), TNamedType(b)) if (a === b) => same
        case (TOption(a), TOption(b))                    => Compare((path, a.some, b.some))

        case (TList(a), TList(b)) => Compare((path / Items, a.some, b.some))
        // According to the spec, Avro ignores the keys' schemas when resolving map schemas
        case (TMap(_, a), TMap(_, b))     => Compare((path / Values, a.some, b.some))
        case (TRequired(a), TRequired(b)) => Compare((path, a.some, b.some))

        case (TContaining(a), TContaining(b)) => CompareList(zipLists(path, a, b, Alternative))
        case (TEither(l1, r1), TEither(l2, r2)) =>
          CompareBoth((path / LeftBranch, l1.some, l2.some), (path / RightBranch, r1.some, r2.some))
        case (TGeneric(g, p), TGeneric(g2, p2)) =>
          CompareList((path / GenericType, g.some, g2.some) :: zipLists(path, p, p2, GenericParameter))
        case (TCoproduct(i), TCoproduct(i2)) =>
          AlignUnionMembers(
            i.zipWithIndex
              .map {
                case (item, idx) =>
                  idx -> (List(item.some), i2.toList.map(_.some)).tupled.map(p => (path / Alternative(idx), p._1, p._2))
              }
              .toList
              .toMap)
        case (TSum(n, f), TSum(n2, f2)) if (n === n2 && f.forall(f2.toSet)) => same
        case (TProduct(n, f), TProduct(n2, f2)) if (n === n2)               => CompareList(zipFields(path / Name(n), f, f2))

        case (_, TCoproduct(i2)) => MatchInList(i2.toList.toVector.map(i => (path, w, i.some)))

        case _ => Result(Mismatch(Nil, NonEmptyList.of(Different(path))))
      }
    case (path, None, Some(reader)) => Result(Match(List(Addition(path, reader))))
    case (path, Some(writer), None) => Result(Match(List(Removal(path, writer))))
    case (_, None, None)            => same
  }

  def zipFields[T](path: Path, l: List[MuF.Field[T]], r: List[MuF.Field[T]]): List[Context[T]] = {
    def toMapEntry(field: MuF.Field[T]): (String, T) = field.name -> field.tpe

    val left  = l.map(toMapEntry).toMap
    val right = r.map(toMapEntry).toMap

    val keys = left.keySet ++ right.keySet

    keys.toList.map(k => (path / FieldName(k), left.get(k), right.get(k)))
  }

  def alg[T]: Comparison[T, ComparisonResult[T]] => ComparisonResult[T] = { comp =>
    {

      comp match {
        case Result(res)             => res
        case Compare(res)            => res
        case CompareList(results)    => results.combineAll
        case CompareBoth(res1, res2) => res1 |+| res2
        case AlignUnionMembers(res) =>
          val alignment = res.collect {
            case (idx, results) if (!results.exists(ComparisonResult.isMatch)) =>
              UnionMemberRemoved(Path.empty / Alternative(idx))
          }.toList
          NonEmptyList.fromList(alignment).fold[ComparisonResult[T]](Match(Nil))(Mismatch(Nil, _))
        case MatchInList(res) =>
          if (res.exists(ComparisonResult.isMatch)) Match(Nil)
          else Mismatch(Nil, NonEmptyList.one(Different(Path.empty)))
      }
    }
  }

  def apply[T](writer: T, reader: T)(implicit ev: Basis[MuF, T]) =
    scheme.hylo(Algebra(alg[T]), Coalgebra(coalg[T])).apply((Path.empty, writer.some, reader.some))
}
