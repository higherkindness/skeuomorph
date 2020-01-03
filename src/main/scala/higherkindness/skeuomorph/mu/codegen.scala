/*
 * Copyright 2018-2020 47 Degrees, LLC. <http://www.47deg.com>
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
import scala.reflect.ClassTag

object codegen {

  private implicit class TreeHack(val tree: Tree) extends AnyVal {
    def as[A](implicit tag: ClassTag[A], classifier: Classifier[Tree, A]): A = {
      require(tree.is[A], s"Expected a tree of type ${tag.runtimeClass.getName} but got: $tree (${tree.structure})")
      tree.asInstanceOf[A]
    }
  }

  def optimize[T](t: T)(implicit T: Basis[MuF, T]): T =
    // Apply optimizations to normalise the protocol
    // before converting it to Scala code
    (nestedOptionInCoproduct[T] andThen knownCoproductTypes[T]).apply(t)

  def protocol[T](
      protocol: Protocol[T],
      streamCtor: (Type, Type) => Type.Apply
  )(implicit T: Basis[MuF, T]): Pkg = {
    val packageName = protocol.pkg.getOrElse("proto")
    val packageTerm = packageName.parse[Term].get.as[Term.Ref]

    val objDefn = q"""
    // TODO options (Where should these annotations be attached to? And what are they for?)
    // TODO dependency imports
    object ${Term.Name(protocol.name)} {
      ..${protocol.declarations.flatMap(t => explodeBlock(schema(optimize(t)).as[Stat]))}
      ..${protocol.services.map(s => service(s, streamCtor))}
    }
    """
    Pkg(packageTerm, List(objDefn))
  }

  // A class and its companion object will be wrapped inside a Block (i.e. curly braces).
  // We need to extract them from there and lift them to the same level as other statements.
  private def explodeBlock(stat: Stat): List[Stat] = stat match {
    case Block(stats) => stats
    case other        => List(other)
  }

  def schema[T](decl: T)(implicit T: Basis[MuF, T]): Tree = {

    def identifier(prefix: List[String], name: String): Type = {
      if (prefix.isEmpty)
        Type.Name(name)
      else {
        val path     = prefix.map(Term.Name(_))
        val pathTerm = path.foldLeft[Term](Term.Name("_root_")) { case (acc, name) => Term.Select(acc, name) }
        Type.Select(pathTerm.as[Term.Ref], Type.Name(name))
      }
    }

    // TODO This works but we lose a lot of type information because everything becomes a Tree.
    // We have to do a lot of casting as a result. Is there a better way to do this?
    val algebra: Algebra[MuF, Tree] = Algebra {
      case TNull()                  => t"Null"
      case TDouble()                => t"_root_.scala.Double"
      case TFloat()                 => t"_root_.scala.Float"
      case TInt()                   => t"_root_.scala.Int"
      case TLong()                  => t"_root_.scala.Long"
      case TBoolean()               => t"_root_.scala.Boolean"
      case TString()                => t"_root_.java.lang.String"
      case TByteArray()             => t"_root_.scala.Array[Byte]"
      case TNamedType(prefix, name) => identifier(prefix, name)
      case TOption(value)           => t"_root_.scala.Option[${value.as[Type]}]"
      case TEither(a, b)            => t"_root_.scala.Either[${a.as[Type]}, ${b.as[Type]}]"
      case TMap(Some(key), value)   => t"_root_.scala.Predef.Map[${key.as[Type]}, ${value.as[Type]}]"
      case TMap(None, value) =>
        t"_root_.scala.Predef.Map[_root_.java.lang.String, ${value.as[Type]}]" // Compatibility for Avro
      case TGeneric(generic, tparams) => t"${generic.as[Type]}[..${tparams.map(_.as[Type])}]"
      case TList(value)               => t"_root_.scala.List[${value.as[Type]}]"
      case TContaining(values)        => q"..${values.map(_.as[Stat])}"
      case TRequired(value)           => value
      case TCoproduct(invariants) =>
        invariants.toList.foldRight[Type](t"_root_.shapeless.CNil") {
          case (t, acc) => t"_root_.shapeless.:+:[${t.as[Type]}, ${acc.as[Type]}]"
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
        """
      case TProduct(name, fields) =>
        def arg(f: Field[_]) = {
          val annotation = mod"@_root_.pbdirect.pbIndex(..${f.indices.map(Lit.Int(_))})"
          param"$annotation ${Term.Name(f.name)}: ${Some(f.tpe.asInstanceOf[Type])}"
        }
        val args = fields.map(arg)
        q"@message final case class ${Type.Name(name)}(..$args)"
    }

    scheme.cata(algebra).apply(decl)
  }

  def service[T](srv: Service[T], streamCtor: (Type, Type) => Type.Apply)(implicit T: Basis[MuF, T]): Stat = {
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

    q"""
    @$serviceAnnotation trait ${Type.Name(srv.name)}[F[_]] {
      ..${srv.operations.map(op => operation(op, streamCtor))}
    }
    """
  }

  def operation[T](op: Service.Operation[T], streamCtor: (Type, Type) => Type.Apply)(
      implicit T: Basis[MuF, T]): Decl.Def =
    q"def ${Term.Name(op.name)}(req: ${requestType(op.request, streamCtor)}): ${responseType(op.response, streamCtor)}"

  def requestType[T](opType: Service.OperationType[T], streamCtor: (Type, Type) => Type.Apply)(
      implicit T: Basis[MuF, T]): Type = {
    val tpe = schema(opType.tpe).as[Type]

    if (opType.stream)
      streamCtor(Type.Name("F"), tpe)
    else
      tpe
  }

  def responseType[T](opType: Service.OperationType[T], streamCtor: (Type, Type) => Type.Apply)(
      implicit T: Basis[MuF, T]): Type.Apply = {
    val tpe = schema(opType.tpe).as[Type]

    if (opType.stream)
      streamCtor(Type.Name("F"), tpe)
    else
      t"F[$tpe]"
  }
}
