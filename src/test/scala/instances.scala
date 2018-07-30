package skeuomorph
package avro

import scala.collection.JavaConverters._
import org.apache.avro.{Schema => AvroSchema}
import org.apache.avro.Schema.{Type => AvroType}

import cats.instances.list._
import cats.syntax.traverse._
import org.scalacheck._
import org.scalacheck.cats.implicits._

object instances {

  implicit val avroSchemaArbitrary: Arbitrary[AvroSchema] = Arbitrary {
    def createGen(tpe: AvroType): Gen[AvroSchema] =
      Gen.const(AvroSchema.create(tpe))

    val primitives: Gen[AvroSchema] = Gen.oneOf(
      createGen(AvroType.STRING),
      createGen(AvroType.BOOLEAN),
      createGen(AvroType.BYTES),
      createGen(AvroType.DOUBLE),
      createGen(AvroType.FLOAT),
      createGen(AvroType.INT),
      createGen(AvroType.LONG),
      createGen(AvroType.NULL)
    )

    val nonEmptyString: Gen[String] = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)

    val arrayOrMap: Gen[AvroSchema] =
      Gen.oneOf(primitives.map(AvroSchema.createMap), primitives.map(AvroSchema.createArray))

    val union: Gen[AvroSchema] =
      Gen.nonEmptyContainerOf[Set, AvroSchema](primitives).map(l => AvroSchema.createUnion(l.toList.asJava))

    def field(name: String): Gen[AvroSchema.Field] =
      for {
        schema <- Gen.oneOf(primitives, arrayOrMap, union)
        doc    <- nonEmptyString
      } yield new AvroSchema.Field(name, schema, doc, null.asInstanceOf[Any])

    val record: Gen[AvroSchema] = for {
      name       <- nonEmptyString
      doc        <- nonEmptyString
      namespace  <- nonEmptyString
      fieldNames <- Gen.nonEmptyContainerOf[Set, String](nonEmptyString).map(_.toList)
      fields     <- fieldNames.traverse(field)
    } yield AvroSchema.createRecord(name, doc, namespace, false, fields.asJava)

    Gen.oneOf(primitives, arrayOrMap, union, record)
  }

}
