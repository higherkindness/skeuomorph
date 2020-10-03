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

import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.Term.Block
import higherkindness.droste._
import higherkindness.skeuomorph.mu.MuF._
import higherkindness.skeuomorph.mu.Optimize._
import higherkindness.skeuomorph.{protobuf => pb}
import higherkindness.skeuomorph.Printer.toValidIdentifier

import scala.reflect.ClassTag
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._
import cats.syntax.apply._

object codegen {

  private implicit class TreeSyntax(val tree: Tree) extends AnyVal {
    def as[A](implicit tag: ClassTag[A], classifier: Classifier[Tree, A]): Either[String, A] =
      Either.cond(
        tree.is[A],
        tree.asInstanceOf[A],
        s"Expected a tree of type ${tag.runtimeClass.getName} but got: $tree (${tree.structure})"
      )
  }

  def protocol[T](
      protocol: Protocol[T],
      streamCtor: (Type, Type) => Type.Apply
  )(implicit T: Basis[MuF, T]): Either[String, Pkg] = {

    val packageName = protocol.pkg
      .map(_.split('.').toList.map(toValidIdentifier).mkString("."))
      .getOrElse("proto")
      .parse[Term]
      .toEither
      .leftMap(e => s"Failed to parse package name: $e")
      .flatMap(_.as[Term.Ref])

    val muImport: Either[String, Import] =
      parseImport("import _root_.higherkindness.mu.rpc.protocol._")

    val depImports: Either[String, List[Import]] =
      protocol.imports.traverse(_import)

    def declaration(decl: T): Either[String, List[Stat]] =
      for {
        tree <- schema(decl)
        stat <- tree.as[Stat]
      } yield explodeBlock(stat)

    val declarations: Either[String, List[Stat]] =
      protocol.declarations.flatTraverse(declaration)

    val services: Either[String, List[Stat]] =
      protocol.services.traverse(s => service(s, streamCtor))

    for {
      pkgName <- packageName
      muImp   <- muImport
      depImps <- depImports
      decls   <- declarations
      srvs    <- services
    } yield {
      protocol.name match {
        case Some(protoName) =>
          // If protocol has a name, wrap the declarations and services
          // together in an object with that name
          val objDefn = q"""
          object ${Term.Name(protoName)} {
            ..$depImps
            ..$decls
            ..$srvs
          }
          """
          Pkg(pkgName, List(muImp, objDefn))
        case None =>
          val stats = muImp :: depImps ++ decls ++ srvs
          Pkg(pkgName, stats)
      }
    }
  }

  private def optimize[T](t: T)(implicit T: Basis[MuF, T]): T =
    // Apply optimizations to normalise the protocol
    // before converting it to Scala code
    (nestedOptionInCoproduct[T] andThen knownCoproductTypes[T]).apply(t)

  // A class and its companion object will be wrapped inside a Block (i.e. curly braces).
  // We need to extract them from there and lift them to the same level as other statements.
  private def explodeBlock(stat: Stat): List[Stat] =
    stat match {
      case Block(stats) => stats
      case other        => List(other)
    }

  private def _import[T](depImport: DependentImport[T]): Either[String, Import] =
    parseImport(s"import ${depImport.pkg}.${depImport.protocol}._")

  private def parseImport(stmt: String): Either[String, Import] =
    for {
      stat <- stmt.parse[Stat].toEither.leftMap(e => s"Failed to parse '$stmt' as an import statement: $e")
      imp  <- stat.as[Import]
    } yield imp

  def schema[T](t: T)(implicit T: Basis[MuF, T]): Either[String, Tree] = {

    def identifier(prefix: List[String], name: String): Either[String, Type] = {
      if (prefix.isEmpty)
        Type.Name(name).asRight
      else {
        val path     = prefix.map(Term.Name(_))
        val pathTerm = path.foldLeft[Term](Term.Name("_root_")) { case (acc, name) => Term.Select(acc, name) }
        pathTerm.as[Term.Ref].map(p => Type.Select(p, Type.Name(name)))
      }
    }

    def intModsToType(modifiers: List[pb.IntModifier]): Type =
      modifiers
        .map {
          case pb.Unsigned   => t"_root_.pbdirect.Unsigned"
          case pb.Signed     => t"_root_.pbdirect.Signed"
          case pb.FixedWidth => t"_root_.pbdirect.Fixed"
        }
        .reduceLeft[Type] { case (a, b) =>
          t"$a with $b"
        }

    def intType(x: TInt[Tree]): Type =
      x match {
        case TSimpleInt(`_32`) | TProtobufInt(`_32`, Nil) => t"_root_.scala.Int"
        case TSimpleInt(`_64`) | TProtobufInt(`_64`, Nil) => t"_root_.scala.Long"
        case TProtobufInt(`_32`, modifiers)               => t"_root_.shapeless.tag.@@[_root_.scala.Int, ${intModsToType(modifiers)}]"
        case TProtobufInt(`_64`, modifiers) =>
          t"_root_.shapeless.tag.@@[_root_.scala.Long, ${intModsToType(modifiers)}]"
      }

    val algebra: AlgebraM[Either[String, ?], MuF, Tree] = AlgebraM {
      case TNull()                  => t"_root_.higherkindness.mu.rpc.protocol.Empty.type".asRight
      case TDouble()                => t"_root_.scala.Double".asRight
      case TFloat()                 => t"_root_.scala.Float".asRight
      case x @ TInt(_)              => intType(x).asRight
      case TBoolean()               => t"_root_.scala.Boolean".asRight
      case TString()                => t"_root_.java.lang.String".asRight
      case TByteArray()             => t"_root_.scala.Array[Byte]".asRight
      case TNamedType(prefix, name) => identifier(prefix, name)
      case TOption(value)           => value.as[Type].map(tpe => t"_root_.scala.Option[$tpe]")
      case TEither(a, b) =>
        (a.as[Type], b.as[Type]).mapN { case (aType, bType) => t"_root_.scala.Either[$aType, $bType]" }
      case TMap(Some(key), value) =>
        (key.as[Type], value.as[Type]).mapN { case (kType, vType) => t"_root_.scala.Predef.Map[$kType, $vType]" }
      case TMap(None, value) =>
        value
          .as[Type]
          .map(vType => t"_root_.scala.Predef.Map[_root_.java.lang.String, $vType]") // Compatibility for Avro
      case TGeneric(generic, tparams) =>
        for {
          tpe <- generic.as[Type]
          ts  <- tparams.traverse(_.as[Type])
        } yield t"$tpe[..$ts]"
      case TList(value)        => value.as[Type].map(tpe => t"_root_.scala.List[$tpe]")
      case TContaining(values) => values.traverse(_.as[Stat]).map(ss => q"..$ss")
      case TRequired(value)    => value.asRight
      case TCoproduct(invariants) =>
        invariants.toList.foldRight[Either[String, Type]](t"_root_.shapeless.CNil".asRight) {
          case (t: Tree, acc: Either[String, Type]) =>
            for {
              tType   <- t.as[Type]
              accType <- acc
            } yield t"_root_.shapeless.:+:[$tType, $accType]"
        }
      case TSum(name, fields) =>
        val typeName   = Type.Name(name)
        val fieldDefns = fields.map(f => q"case object ${Term.Name(f.name)} extends $typeName(${f.value})")
        q"""
        sealed abstract class $typeName(val value: _root_.scala.Int) extends _root_.enumeratum.values.IntEnumEntry
        object ${Term.Name(name)} extends _root_.enumeratum.values.IntEnum[$typeName] {
          ..$fieldDefns

          ;
          val values = findValues
        }
        """.asRight
      case TProduct(name, fields, nestedProducts, nestedCoproducts) =>
        def arg(f: Field[Tree]): Either[String, Term.Param] = {
          val annotation = f.indices.map(indices => mod"@_root_.pbdirect.pbIndex(..${indices.map(Lit.Int(_))})")

          val param: Type => Term.Param = { tpe =>
            param"..$annotation ${Term.Name(f.name)}: ${Some(tpe)}"
          }
          f.tpe match {
            case tpe: Type =>
              param(tpe).asRight
            case cls: Defn.Class =>
              param(cls.name).asRight
            case _ => {
              s"Encountered unhandled Tree type: ${f.tpe} in Field: ${f}".asLeft
            }
          }
        }

        (
          fields.traverse(arg),
          nestedProducts.traverse(_.as[Stat]),
          nestedCoproducts.traverse(_.as[Stat])
        ).mapN { case (args, prods, coprods) =>
          val caseClass = q"final case class ${Type.Name(name)}(..$args)"
          if (prods.nonEmpty || coprods.nonEmpty) {
            q"""
            $caseClass
            ;
            object ${Term.Name(name)} {
              ..${prods.flatMap(explodeBlock)}
              ..${coprods.flatMap(explodeBlock)}
            }
            """
          } else
            caseClass
        }
      case TDate()    => t"_root_.java.time.LocalDate".asRight
      case TInstant() => t"_root_.java.time.Instant".asRight
      case TUUID()    => t"_root_.java.util.UUID".asRight
      case TDecimal(precision, scale) =>
        t"_root_.shapeless.tag.@@[_root_.scala.math.BigDecimal, ((${Lit.String("precision")}, ${Lit
          .Int(precision)}), (${Lit.String("scale")}, ${Lit.Int(scale)}))]".asRight
    }

    scheme.cataM(algebra).apply(optimize(t))
  }

  def service[T](srv: Service[T], streamCtor: (Type, Type) => Type.Apply)(implicit
      T: Basis[MuF, T]
  ): Either[String, Stat] = {
    val serializationType = Term.Name(srv.serializationType.toString)
    val compressionType   = Term.Name(srv.compressionType.toString)

    val serviceAnnotation = srv.idiomaticEndpoints match {
      case IdiomaticEndpoints(Some(pkg), true) =>
        mod"@service($serializationType, $compressionType, namespace = Some($pkg), methodNameStyle = Capitalize)"
      case IdiomaticEndpoints(None, true) =>
        mod"@service($serializationType, $compressionType, methodNameStyle = Capitalize)"
      case _ =>
        mod"@service($serializationType, $compressionType)"
    }

    srv.operations.traverse(op => operation(op, streamCtor)).map { ops =>
      q"""
      @$serviceAnnotation trait ${Type.Name(srv.name)}[F[_]] {
        ..$ops
      }
      """
    }
  }

  def operation[T](op: Service.Operation[T], streamCtor: (Type, Type) => Type.Apply)(implicit
      T: Basis[MuF, T]
  ): Either[String, Decl.Def] =
    for {
      reqType  <- requestType(op.request, streamCtor)
      respType <- responseType(op.response, streamCtor)
    } yield q"def ${Term.Name(op.name)}(req: $reqType): $respType"

  def requestType[T](opType: Service.OperationType[T], streamCtor: (Type, Type) => Type.Apply)(implicit
      T: Basis[MuF, T]
  ): Either[String, Type] =
    for {
      tree <- schema(opType.tpe)
      tpe  <- tree.as[Type]
    } yield {
      if (opType.stream)
        streamCtor(Type.Name("F"), tpe)
      else
        tpe
    }

  def responseType[T](opType: Service.OperationType[T], streamCtor: (Type, Type) => Type.Apply)(implicit
      T: Basis[MuF, T]
  ): Either[String, Type.Apply] =
    for {
      tree <- schema(opType.tpe)
      tpe  <- tree.as[Type]
    } yield {
      if (opType.stream) {
        val streamType = streamCtor(Type.Name("F"), tpe)
        t"F[$streamType]"
      } else
        t"F[$tpe]"
    }

}
