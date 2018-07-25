package skeuomorph
package avro

import scala.collection.JavaConverters._
import cats.data.NonEmptyList
import turtles._
import org.apache.avro.{Schema => AvroSchema}
import org.apache.avro.Schema.{Type => AvroType}

object util {

  def order2Order(avroO: AvroSchema.Field.Order): Order = avroO match {
    case AvroSchema.Field.Order.ASCENDING  => Order.Ascending
    case AvroSchema.Field.Order.DESCENDING => Order.Descending
    case AvroSchema.Field.Order.IGNORE     => Order.Ignore
  }

  def field2Field(avroF: AvroSchema.Field): Field[AvroSchema] = Field(
    avroF.name,
    avroF.aliases.asScala.toList,
    Option(avroF.doc),
    Option(order2Order(avroF.order)),
    fromAvroAvroSchema(avroF.schema)
  )

  /**
   * Convert [[org.apache.avro.Schema]] to [[skeuomorph.avro.AvroSchema]]
   */
  def fromAvroAvroSchema: Coalgebra[Schema, AvroSchema] = { sch =>
    sch.getType match {
      case AvroType.STRING  => Schema.TString()
      case AvroType.BOOLEAN => Schema.TBoolean()
      case AvroType.BYTES   => Schema.TBytes()
      case AvroType.DOUBLE  => Schema.TDouble()
      case AvroType.FLOAT   => Schema.TFloat()
      case AvroType.INT     => Schema.TInt()
      case AvroType.LONG    => Schema.TLong()
      case AvroType.NULL    => Schema.TNull()
      case AvroType.MAP     => Schema.TMap(sch.getValueType)
      case AvroType.ARRAY   => Schema.TArray(sch.getElementType)
      case AvroType.RECORD =>
        Schema.TRecord(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          Option(sch.getDoc),
          sch.getFields.asScala.toList.map(field2Field)
        )
      case AvroType.ENUM =>
        val symbols = sch.getEnumSymbols.asScala.toList
        Schema.TEnum(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          Option(sch.getDoc),
          symbols,
          i => symbols.map(sch.getEnumOrdinal).zip(symbols).toMap.get(i)
        )
      case AvroType.UNION =>
        val types = sch.getTypes.asScala.toList
        Schema.TUnion(
          NonEmptyList.fromListUnsafe(types),
          i => types.map(x => sch.getIndexNamed(x.getName)).zip(types).toMap.get(i)
        )
      case AvroType.FIXED =>
        Schema.TFixed(
          sch.getName,
          Option(sch.getNamespace),
          sch.getAliases.asScala.toList,
          sch.getFixedSize
        )
    }
  }
}
