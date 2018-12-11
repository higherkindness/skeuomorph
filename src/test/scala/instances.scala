/*
 * Copyright 2018 47 Degrees, LLC. <http://www.47deg.com>
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

package skeuomorph
package avro
package protobuf

import cats.data.NonEmptyList
import cats.implicits._
import com.google.protobuf.descriptor.DescriptorProto.ReservedRange
import com.google.protobuf.descriptor.FieldDescriptorProto.{Label, Type}
import com.google.protobuf.descriptor._
import com.google.protobuf.descriptor.FieldOptions.{CType, JSType}
import org.apache.avro.Schema
import org.scalacheck._
import org.scalacheck.cats.implicits._
import qq.droste.Basis
import scalapb.UnknownFieldSet
import scalapb.descriptors.FileDescriptor
import skeuomorph.mu.MuF

import scala.collection.JavaConverters._

object instances {

  val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

  val smallNumber: Gen[Int] = Gen.choose(1, 10)

  val sampleUninterpretedOption: Arbitrary[UninterpretedOption] = Arbitrary {
    new UninterpretedOption() // TODO
  }

  val sampleMessageOptionProto: Arbitrary[MessageOptions] = Arbitrary {
    new MessageOptions(messageSetWireFormat = Some(false),
                               noStandardDescriptorAccessor = Some(false),
                               deprecated = Some(false),
                               mapEntry = None,
                               uninterpretedOption = Seq(), //TODO: See above
                               unknownFields = UnknownFieldSet())
  }

  val labelGenerator: Gen[Label] = Gen.oneOf(Seq(Label.LABEL_OPTIONAL, Label.LABEL_REPEATED, Label.LABEL_REQUIRED))
  
  
  val fieldTypeGenerator: Gen[FieldDescriptorProto.Type] = Gen.oneOf(
    Seq(
      Type.TYPE_DOUBLE,
      Type.TYPE_FLOAT,
      Type.TYPE_INT32,            
      Type.TYPE_INT64,            
      Type.TYPE_UINT32,            
      Type.TYPE_UINT64,          
      Type.TYPE_SINT32,          
      Type.TYPE_SINT64,           
      Type.TYPE_FIXED32,           
      Type.TYPE_FIXED64,          
      Type.TYPE_SFIXED32,          
      Type.TYPE_SFIXED64,             
      Type.TYPE_BOOL,                
      Type.TYPE_STRING,                
      Type.TYPE_BYTES,             
      Type.TYPE_GROUP,    // Under what condition do these get generated?
      Type.TYPE_MESSAGE,
      Type.TYPE_ENUM      
    )
  )

  // TODO: create instances of trait not object.
  val sampleCType = Arbitrary(CType.CORD)
  val sampleJsType = Arbitrary(JSType)

  val sampleFieldOptions: Arbitrary[FieldOptions] = Arbitrary {
    for {
      _                    <- Gen.option(sampleCType.arbitrary)
      packed               <- Gen.option(Arbitrary.arbBool.arbitrary)
      _                    <- Gen.option(sampleJsType.arbitrary)
      lazyf                <- Gen.option(Arbitrary.arbBool.arbitrary)
      deprecated           <- Gen.option(Arbitrary.arbBool.arbitrary)
      weak                 <- Gen.option(Arbitrary.arbBool.arbitrary)
    } yield
      new FieldOptions(
        ctype = None,
        packed,
        jstype = None,
        lazyf,
        deprecated,
        weak,
        uninterpretedOption = Seq(), // TODO: see above
        unknownFields = UnknownFieldSet()
      )
  }
  
  val sampleFieldDescProto: Arbitrary[FieldDescriptorProto] = Arbitrary {
    for {
      name <- nonEmptyString
      number <- smallNumber
      label <- Gen.option(labelGenerator)
      fieldType <- fieldTypeGenerator
      options <- Gen.option(sampleFieldOptions.arbitrary)
    } yield
      new FieldDescriptorProto(
        Some(name),
        Some(number),
        label,
        Some(fieldType),
        Some(fieldType.name),
        extendee = None, // ?? 
        defaultValue = None, // ?? see spec guide on this
        oneofIndex = None,
        Some(name),
        options
      )
  }

  val sampleReservedRangeProto: Arbitrary[ReservedRange] = Arbitrary {
    for {
      maybeStart <- Gen.option(smallNumber)
      maybeEnd = maybeStart.map(_ + 1)
    } yield new ReservedRange(maybeStart, maybeEnd)
  }

  val sampleDescriptorProto: Arbitrary[DescriptorProto] = Arbitrary {
    for {
      name <- nonEmptyString
      fields <- Gen.containerOf[Seq, FieldDescriptorProto](sampleFieldDescProto.arbitrary)
      oneOrZero <- Gen.choose(0,1)
      _ <- Gen.lzy(Gen.containerOfN[Seq, DescriptorProto](oneOrZero, sampleDescriptorProto.arbitrary))
//      enumTypes <- Gen.some(???)
      messageOptions   <- Gen.option(sampleMessageOptionProto.arbitrary)
      reservedRange    <- Gen.containerOf[Seq, ReservedRange](sampleReservedRangeProto.arbitrary)
      reservedNames    <- Gen.containerOf[Seq, String](nonEmptyString)
    } yield
      new DescriptorProto(
        name = Some(name),
        field = fields,
        extension = Seq(),
        nestedType = Seq(),
        enumType = Seq(),
        extensionRange = Seq(),
        oneofDecl = Seq(), // TODO?
        options = messageOptions,
        reservedRange = reservedRange,
        reservedName = reservedNames
      )
  }

  val sampleFileDescriptorProto: Arbitrary[FileDescriptorProto] = Arbitrary {
    for {
      name <- Gen.option(nonEmptyString)
      packageN <- Gen.option(nonEmptyString)
      messages <- Gen.containerOf[Seq, DescriptorProto](sampleDescriptorProto.arbitrary)
//      enums <- Gen.someOf(???)
//      services <- Gen.someOf(???)
//      extensions <- ???
    } yield 
      new FileDescriptorProto(
        name = name,
        `package` = packageN,
        dependency =  Seq(), // TODO
        publicDependency = Seq(), // TODO
        weakDependency = Seq(),
        messageType = messages,
        enumType = Seq(),
        service = Seq(),
        extension = Seq(),
        options = None,
        sourceCodeInfo = None,
        syntax = Some("proto3")
      )
  }
  
  
  implicit val baseDescriptorArbitrary: Arbitrary[FileDescriptor] = Arbitrary {
    for {
      sampleFileDescriptorProto <- Gen.lzy(sampleFileDescriptorProto.arbitrary)
    } yield FileDescriptor.buildFrom(sampleFileDescriptorProto, Nil)
  }

  implicit val avroSchemaArbitrary: Arbitrary[Schema] = Arbitrary {
    val primitives: Gen[Schema] = Gen.oneOf(
      List(
        org.apache.avro.Schema.Type.STRING,
        org.apache.avro.Schema.Type.BOOLEAN,
        org.apache.avro.Schema.Type.BYTES,
        org.apache.avro.Schema.Type.DOUBLE,
        org.apache.avro.Schema.Type.FLOAT,
        org.apache.avro.Schema.Type.INT,
        org.apache.avro.Schema.Type.LONG,
        org.apache.avro.Schema.Type.NULL
      ).map(Schema.create)
    )

    val arrayOrMap: Gen[Schema] =
      Gen.oneOf(primitives.map(Schema.createMap), primitives.map(Schema.createArray))

    val union: Gen[Schema] =
      Gen.nonEmptyContainerOf[Set, Schema](primitives).map(l => Schema.createUnion(l.toList.asJava))

    def field(name: String): Gen[Schema.Field] =
      for {
        schema <- Gen.oneOf(primitives, arrayOrMap, union)
        doc    <- nonEmptyString
      } yield new Schema.Field(name, schema, doc, null.asInstanceOf[Any])

    val record: Gen[Schema] = (
      nonEmptyString,
      nonEmptyString,
      nonEmptyString,
      Gen.nonEmptyContainerOf[Set, String](nonEmptyString).map(_.toList) flatMap { l: List[String] =>
        l.traverse(field)
      }
    ).mapN {
      case (name, doc, namespace, fields) =>
        Schema.createRecord(name, doc, namespace, false, fields.asJava)
    }

    Gen.oneOf(primitives, arrayOrMap, union, record)
  }

  implicit def muCoproductArbitrary[T](withTNull: Boolean)(implicit B: Basis[MuF, T]): Arbitrary[MuF.TCoproduct[T]] =
    Arbitrary {
      val nonNullPrimitives: Gen[MuF[T]] = Gen.oneOf(
        List(
          MuF.TString[T](),
          MuF.TBoolean[T](),
          MuF.TByteArray[T](),
          MuF.TDouble[T](),
          MuF.TFloat[T](),
          MuF.TInt[T](),
          MuF.TLong[T]()
        )
      )

      (
        nonNullPrimitives,
        if (withTNull) Gen.const(MuF.TNull[T]()) else nonNullPrimitives,
        Gen.oneOf(true, false)
      ).mapN((t1, t2, reversed) =>
        MuF.TCoproduct(if (reversed) NonEmptyList.of(B.algebra(t2), B.algebra(t1))
        else NonEmptyList.of(B.algebra(t1), B.algebra(t2))))
    }

  def muCoproductWithTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = true).arbitrary

  def muCoproductWithoutTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = false).arbitrary
}
