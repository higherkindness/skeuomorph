package skeuomorph
package freestyle

import protobuf.{Schema => ProtoSchema}
import turtles._
import turtles.implicits._

object utils {

  import Schema._

  def transformProto[A]: ProtoSchema[A] => Schema[A] = {
    case ProtoSchema.TDouble()                  => TDouble()
    case ProtoSchema.TFloat()                   => TFloat()
    case ProtoSchema.TInt32()                   => TInt()
    case ProtoSchema.TInt64()                   => TLong()
    case ProtoSchema.TUint32()                  => TInt()
    case ProtoSchema.TUint64()                  => TLong()
    case ProtoSchema.TSint32()                  => TInt()
    case ProtoSchema.TSint64()                  => TLong()
    case ProtoSchema.TFixed32()                 => TInt()
    case ProtoSchema.TFixed64()                 => TLong()
    case ProtoSchema.TSfixed32()                => TInt()
    case ProtoSchema.TSfixed64()                => TLong()
    case ProtoSchema.TBool()                    => TBoolean()
    case ProtoSchema.TString()                  => TString()
    case ProtoSchema.TBytes()                   => TByteArray()
    case ProtoSchema.TNamedType(name)           => TNamedType(name)
    case ProtoSchema.TOptional(value)           => TOption(value)
    case ProtoSchema.TRepeated(value)           => TList(value)
    case ProtoSchema.TRequired(value)           => TRequired(value)
    case ProtoSchema.TEnum(name, symbols, _, _) => TSum(name, symbols.map(_._1))
    case ProtoSchema.TMessage(name, fields, _)  => TProduct(name, fields.map(f => Field(f.name, f.tpe)))
  }

  /**
   * micro-optimization to convert types from fields in a product to
   * NamedTypes.
   *
   * Without this optimization, printing a product containing fields
   * of other products would end up with something like:
   *
   * {{{
   * case class Product(field1: String, field2: case class OtherField())
   * }}}
   *
   * With it, we cut recursion in messages, to leave only type names:
   *
   * {{{
   * case class Product(field1: String, field2: OtherField)
   * }}}
   */
  def namedTypes[T](t: T)(implicit T: Birecursive.Aux[T, Schema]): T =
    t.project match {
      case TProduct(name, fields) =>
        TProduct[T](
          name,
          fields.map(f =>
            f.copy(tpe = f.tpe.transCataT(_.project match {
              case TProduct(name, _) => TNamedType[T](name).embed
              case TSum(name, _)     => TNamedType[T](name).embed
              case other             => T.embed(other)
            })))
        ).embed
      case other => T.embed(other)
    }

  def render: Algebra[Schema, String] = {
    case TDouble()        => "Double"
    case TFloat()         => "Float"
    case TInt()           => "Int"
    case TLong()          => "Long"
    case TBoolean()       => "Boolean"
    case TString()        => "String"
    case TByteArray()     => "Array[Byte]"
    case TNamedType(name) => name
    case TOption(value)   => s"Option[$value]"
    case TList(value)     => s"List[$value]"
    case TRequired(value) => value
    case TSum(name, fields) =>
      val printFields = fields.map(f => s"case object $f extends $name").mkString("\n")
      s"""
      sealed trait $name
      object $name {
        $printFields
      }
      """
    case TProduct(name, fields) =>
      val printFields = fields.map(f => s"${f.name}: ${f.tpe}").mkString(", ")
      s"""
      @message case class $name($printFields)
      """
  }

  /**
   * convert between protobuf & freestyle schemas
   */
  def fromProtoRender[T, U](t: T)(
      implicit
      T: Birecursive.Aux[T, ProtoSchema],
      U: Birecursive.Aux[U, Schema]): String =
    t.transCata[U](transformProto).transCataT(namedTypes).cata(render)

}
