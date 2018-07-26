package skeuomorph
package freestyle

import protobuf.{Schema => ProtoSchema}
import avro.{Schema => AvroSchema}
import turtles._
import turtles.implicits._

object utils {

  import Schema._

  /**
   * transform Protobuf schema into Freestyle schema
   */
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

  def transformAvro[A]: AvroSchema[A] => Schema[A] = {
    case AvroSchema.TNull()          => TNull()
    case AvroSchema.TBoolean()       => TBoolean()
    case AvroSchema.TInt()           => TInt()
    case AvroSchema.TLong()          => TLong()
    case AvroSchema.TFloat()         => TFloat()
    case AvroSchema.TDouble()        => TDouble()
    case AvroSchema.TBytes()         => TByteArray()
    case AvroSchema.TString()        => TString()
    case AvroSchema.TNamedType(name) => TNamedType(name)
    case AvroSchema.TArray(item)     => TList(item)
    case AvroSchema.TMap(values)     => TMap(values)
    case AvroSchema.TRecord(name, _, _, _, fields) =>
      TProduct(name, fields.map(f => Field(f.name, f.tpe)))
    case AvroSchema.TEnum(name, _, _, _, symbols) => TSum(name, symbols)
    case AvroSchema.TUnion(options)               => TCoproduct(options)
    case AvroSchema.TFixed(_, _, _, _) =>
      ??? // I don't really know what to do with Fixed... https://avro.apache.org/docs/current/spec.html#Fixed
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
          fields.map { f: Field[T] =>
            f.copy(tpe = f.tpe.transCataT(_.project match {
              case TProduct(name, _) => TNamedType[T](name).embed
              case TSum(name, _)     => TNamedType[T](name).embed
              case other             => other.embed
            }))
          }
        ).embed
      case other => other.embed
    }

  def render: Algebra[Schema, String] = {
    case TNull()          => "Null"
    case TDouble()        => "Double"
    case TFloat()         => "Float"
    case TInt()           => "Int"
    case TLong()          => "Long"
    case TBoolean()       => "Boolean"
    case TString()        => "String"
    case TByteArray()     => "Array[Byte]"
    case TNamedType(name) => name
    case TOption(value)   => s"Option[$value]"
    case TMap(value)      => s"Map[String, $value]"
    case TList(value)     => s"List[$value]"
    case TRequired(value) => value
    case TCoproduct(invariants) =>
      invariants.toList.mkString("Cop[", " :: ", ":: Tnil]")
    case TSum(name, fields) =>
      val printFields = fields.map(f => s"case object $f extends $name").mkString("\n  ")
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
