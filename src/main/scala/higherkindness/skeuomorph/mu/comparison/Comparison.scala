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

package higherkindness.skeuomorph.mu.comparison

import qq.droste._
import qq.droste.syntax.project._
import higherkindness.skeuomorph.mu.MuF, MuF._
import cats.Functor
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.apply._

object Reporter {

  import ComparisonResult._
  import Transformation._

  def id[T]: ComparisonResult[T] => ComparisonResult[T] = r => r

  def madeOptional[T](path: Path): ComparisonResult[T] => ComparisonResult[T] = {
    case Match(tr) => Match(MadeOptional[T](path) +: tr)
    case mismatch  => mismatch
  }

  def promotedToEither[T](path: Path, either: T): ComparisonResult[T] => ComparisonResult[T] = {
    case Match(tr) => Match(PromotedToEither(path, either) :: tr)
    case mismatch  => mismatch
  }

  def promotedToCoproduct[T](path: Path, coproduct: T): ComparisonResult[T] => ComparisonResult[T] = {
    case Match(tr) => Match(PromotedToCoproduct(path, coproduct) :: tr)
    case mismatch  => mismatch
  }

}

sealed trait Comparison[T, A]
object Comparison {

  import ComparisonResult._
  import PathElement._
  import Transformation._
  import Incompatibility._

  type Reporter[T] = ComparisonResult[T] => ComparisonResult[T]

  final case class Result[T, A](result: ComparisonResult[T])                                 extends Comparison[T, A]
  final case class Compare[T, A](a: A, reporter: Reporter[T] = Reporter.id[T])               extends Comparison[T, A]
  final case class CompareBoth[T, A](x: A, y: A)                                             extends Comparison[T, A]
  final case class CompareList[T, A](items: List[A], reporter: Reporter[T] = Reporter.id[T]) extends Comparison[T, A]
  final case class MatchInList[T, A](attempts: Vector[A], reporter: Reporter[T] = Reporter.id[T])
      extends Comparison[T, A]
  final case class AlignUnionMembers[T, A](attempts: Map[Path, List[A]], reporter: Reporter[T] = Reporter.id[T])
      extends Comparison[T, A]

  implicit def comparisonCatsFunctor[T] = new Functor[Comparison[T, ?]] {
    def map[A, B](fa: Comparison[T, A])(f: (A) => B): Comparison[T, B] = fa match {
      case Result(res)               => Result(res)
      case Compare(a, rep)           => Compare(f(a), rep)
      case CompareBoth(x, y)         => CompareBoth(f(x), f(y))
      case CompareList(i, rep)       => CompareList(i.map(f), rep)
      case MatchInList(a, rep)       => MatchInList(a.map(f), rep)
      case AlignUnionMembers(a, rep) => AlignUnionMembers(a.mapValues(_.map(f)), rep)
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
        case (TByteArray(), TString()) | (TString(), TByteArray()) =>
          Result(Match(List(StringConversion(path, writer, reader))))

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
                  path / Alternative(idx) -> (List(item.some), i2.toList.map(_.some)).tupled.map(p =>
                    (path / Alternative(idx), p._1, p._2))
              }
              .toList
              .toMap)
        case (TSum(n, f), TSum(n2, f2)) if (n === n2 && f.forall(f2.toSet)) => same
        case (TProduct(n, f), TProduct(n2, f2)) if (n === n2)               => CompareList(zipFields(path / Name(n), f, f2))

        case (TOption(i1), TCoproduct(is)) =>
          MatchInList(is.toList.toVector.map(i => (path, i1.some, i.some)), Reporter.promotedToCoproduct(path, reader))

        case (TOption(i1), TEither(r1, r2)) =>
          MatchInList(
            Vector((path, i1.some, r1.some), (path, i1.some, r2.some)),
            Reporter.promotedToEither(path, reader))

        case (TEither(l1, r1), TCoproduct(rs)) =>
          AlignUnionMembers(
            Map(
              path / LeftBranch  -> rs.toList.map(rr => (path / LeftBranch, l1.some, rr.some)),
              path / RightBranch -> rs.toList.map(rr => (path / RightBranch, r1.some, rr.some)),
            ),
            Reporter.promotedToCoproduct(path, reader)
          )

        case (_, TCoproduct(i2)) =>
          MatchInList(i2.toList.toVector.map(i => (path, w, i.some)), Reporter.promotedToCoproduct(path, reader))
        case (_, TOption(t2)) =>
          Compare((path, w, t2.some), Reporter.madeOptional[T](path))
        case (_, TEither(l2, r2)) =>
          MatchInList(Vector((path, w, l2.some), (path, w, r2.some)), Reporter.promotedToEither(path, reader))
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

  def alg[T]: Comparison[T, ComparisonResult[T]] => ComparisonResult[T] = {
    case Result(res)               => res
    case Compare(res, rep)         => rep(res)
    case CompareList(results, rep) => rep(results.combineAll)
    case CompareBoth(res1, res2)   => res1 |+| res2
    case AlignUnionMembers(res, rep) =>
      rep(
        res
          .map {
            case (p, results) =>
              results
                .find(ComparisonResult.isMatch)
                .getOrElse(Mismatch(Nil, NonEmptyList.one(UnionMemberRemoved(p))))
          }
          .toList
          .combineAll)

    case MatchInList(res, rep) =>
      val firstMatch = res.find(ComparisonResult.isMatch)
      val searchResult =
        firstMatch.getOrElse(Mismatch(Nil, NonEmptyList.one(Different(Path.empty))))
      rep(searchResult)
  }

  def apply[T](writer: T, reader: T)(implicit ev: Basis[MuF, T]) =
    scheme.hylo(Algebra(alg[T]), Coalgebra(coalg[T])).apply((Path.empty, writer.some, reader.some))
}
