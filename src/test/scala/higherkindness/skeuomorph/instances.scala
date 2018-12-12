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

package higherkindness.skeuomorph

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
import higherkindness.skeuomorph.mu.MuF

import scala.collection.JavaConverters._

object instances {

  lazy val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

  lazy val smallNumber: Gen[Int] = Gen.choose(1, 10)

  lazy val sampleUninterpretedOption: Arbitrary[UninterpretedOption] = Arbitrary {
    new UninterpretedOption() // TODO
  }

  lazy val sampleMessageOptionProto: Arbitrary[MessageOptions] = Arbitrary {
    new MessageOptions(
      messageSetWireFormat = Some(false),
      noStandardDescriptorAccessor = Some(false),
      deprecated = Some(false),
      mapEntry = None,
      uninterpretedOption = Seq(), //TODO: See above
      unknownFields = UnknownFieldSet()
    )
  }

  lazy val labelGenerator: Gen[Label] = Gen.oneOf(Seq(Label.LABEL_REPEATED))

  /* Type.TYPE_ENUM and Type.TYPE_MESSAGE are valid types but if added to these generators
    they break the scalaPB parser. Type.TYPE_GROUP is not valid in Proto3 and therefore should
    not be generated */
  lazy val fieldTypeGenerator: Gen[FieldDescriptorProto.Type] = Gen.oneOf(
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
    )
  )

  lazy val sampleFieldOptions: Arbitrary[FieldOptions] = Arbitrary {
    for {
      cTypeEnum  <- Gen.choose(0, 2)
      cType      <- Gen.option(CType.fromValue(cTypeEnum))
      packed     <- Gen.option(Arbitrary.arbBool.arbitrary)
      jsTimeEnum <- Gen.choose(0, 2)
      jsType     <- Gen.option(JSType.fromValue(jsTimeEnum))
      lazyf      <- Gen.option(Arbitrary.arbBool.arbitrary)
      deprecated <- Gen.option(Arbitrary.arbBool.arbitrary)
      weak       <- Gen.option(Arbitrary.arbBool.arbitrary)
    } yield
      new FieldOptions(
        ctype = cType,
        packed,
        jstype = jsType,
        lazyf,
        deprecated,
        weak,
        uninterpretedOption = Seq(), // TODO: see above
        unknownFields = UnknownFieldSet()
      )
  }

  def sampleFieldDescProto(packageName: String, messageName: String): Arbitrary[FieldDescriptorProto] = Arbitrary {
    for {
      name      <- nonEmptyString
      number    <- smallNumber
      label     <- Gen.option(labelGenerator)
      fieldType <- Gen.lzy(fieldTypeGenerator)
      options   <- Gen.lzy(Gen.option(sampleFieldOptions.arbitrary))
    } yield
      new FieldDescriptorProto(
        Some(name),
        Some(number),
        label,
        Some(fieldType),
        Some(s".$packageName.$messageName"),
        extendee = None, // ??
        defaultValue = None, // ?? see spec guide on this
        oneofIndex = None,
        Some(name),
        options
      )
  }

  lazy val sampleReservedRangeProto: Arbitrary[ReservedRange] = Arbitrary {
    for {
      maybeStart <- Gen.option(smallNumber)
      maybeEnd = maybeStart.map(_ + 1)
    } yield new ReservedRange(maybeStart, maybeEnd)
  }

  lazy val sampleEnumValueDescriptor: Arbitrary[EnumValueDescriptorProto] = Arbitrary {
    for {
      name   <- nonEmptyString
      number <- smallNumber
//      options = Gen.option() // TODO
    } yield
      new EnumValueDescriptorProto(
        name = Some(name),
        number = Some(number),
        options = None
      )
  }

  lazy val sampleEnumDescriptor: Arbitrary[EnumDescriptorProto] = Arbitrary {
    for {
      name                  <- nonEmptyString
      valueDescriptorLength <- Gen.choose(1, 3)
      enumValues <- Gen.lzy(
        Gen.containerOfN[Seq, EnumValueDescriptorProto](valueDescriptorLength, sampleEnumValueDescriptor.arbitrary))
//      options <- Gen.option() // TODO
    } yield
      new EnumDescriptorProto(
        name = Some(name),
        value = enumValues,
        options = None
      )
  }

  def sampleDescriptorProto(packageName: String): Arbitrary[DescriptorProto] = Arbitrary {
    for {
      name      <- nonEmptyString
      oneOrZero <- Gen.choose(0, 1)
      messageName = name
      fields <- Gen.lzy(
        Gen.containerOfN[Seq, FieldDescriptorProto](10, sampleFieldDescProto(packageName, messageName).arbitrary))
      nestedTypes <- Gen.lzy(
        Gen.containerOfN[Seq, DescriptorProto](oneOrZero, sampleDescriptorProto(packageName).arbitrary))
      enums          <- Gen.lzy(Gen.containerOfN[Seq, EnumDescriptorProto](oneOrZero, sampleEnumDescriptor.arbitrary))
      messageOptions <- Gen.lzy(Gen.option(sampleMessageOptionProto.arbitrary))
      reservedRange  <- Gen.lzy(Gen.containerOfN[Seq, ReservedRange](oneOrZero, sampleReservedRangeProto.arbitrary))
      reservedNames  <- Gen.lzy(Gen.containerOfN[Seq, String](oneOrZero, nonEmptyString))
    } yield
      new DescriptorProto(
        name = Some(messageName),
        field = fields,
        extension = Seq(),
        nestedType = nestedTypes,
        enumType = enums,
        extensionRange = Seq(),
        oneofDecl = Seq(), // TODO?
        options = messageOptions,
        reservedRange = reservedRange,
        reservedName = reservedNames
      )
  }

  lazy val sampleFileDescriptorProto: Arbitrary[FileDescriptorProto] = Arbitrary {
    for {
      name                 <- nonEmptyString
      packageN             <- nonEmptyString
      messageAndEnumLength <- Gen.choose(1, 5)
      messages <- Gen.lzy(
        Gen.containerOfN[Seq, DescriptorProto](messageAndEnumLength, sampleDescriptorProto(packageN).arbitrary))
      enums <- Gen.lzy(Gen.containerOfN[Seq, EnumDescriptorProto](messageAndEnumLength, sampleEnumDescriptor.arbitrary))
    } yield
      new FileDescriptorProto(
        name = Some(name),
        `package` = Some(packageN),
        dependency = Seq(), // TODO
        publicDependency = Seq(), // TODO
        weakDependency = Seq(),
        messageType = messages,
        enumType = enums,
        service = Seq(),
        extension = Seq(),
        options = None,
        sourceCodeInfo = None,
        syntax = Some("proto3")
      )
  }

  implicit lazy val baseDescriptorArbitrary: Arbitrary[FileDescriptor] = Arbitrary {
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
