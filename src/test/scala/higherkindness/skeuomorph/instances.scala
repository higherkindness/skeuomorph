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
import com.google.protobuf.ByteString
import com.google.protobuf.descriptor.DescriptorProto.ReservedRange
import com.google.protobuf.descriptor.FieldDescriptorProto.{Label, Type}
import com.google.protobuf.descriptor._
import com.google.protobuf.descriptor.UninterpretedOption.NamePart
import org.apache.avro.Schema
import org.scalacheck._
import org.scalacheck.cats.implicits._
import qq.droste.Basis
import scalapb.UnknownFieldSet
import scalapb.descriptors.FileDescriptor
import higherkindness.skeuomorph.mu.MuF
import higherkindness.skeuomorph.protobuf.ProtobufF

import scala.collection.JavaConverters._

object instances {

  lazy val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

  lazy val smallNumber: Gen[Int] = Gen.choose(1, 10)

  lazy val sampleBool: Gen[Boolean] = Gen.oneOf(true, false)

  lazy val uninterpretedOptionNamePart = Arbitrary {
    for {
      name <- nonEmptyString
      isExtension = false // These generators will not support Extensions as that is a Proto2 concept
    } yield new NamePart(name, isExtension)
  }

  val uninterpretedOptionSample: Arbitrary[UninterpretedOption] = Arbitrary {
    for {
      name             <- Gen.containerOfN[Seq, NamePart](2, uninterpretedOptionNamePart.arbitrary)
      identifierValue  <- nonEmptyString
      positiveIntValue <- Gen.option(smallNumber.map(_.toLong))
      negativeIntValue <- Gen.option(smallNumber.map(_.toLong * -1))
      doubleValue      <- Gen.option(Gen.oneOf(2.0, 3.0))
      stringValue      <- Gen.option(nonEmptyString.map(s => ByteString.copyFrom(s.getBytes("UTF-8"))))
    } yield
      new UninterpretedOption(
        name = name,
        identifierValue = Some(identifierValue),
        positiveIntValue = positiveIntValue,
        negativeIntValue = negativeIntValue,
        doubleValue = doubleValue,
        stringValue = stringValue,
        aggregateValue = None
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
      Type.TYPE_BYTES
    )
  )

  lazy val sampleFieldOptions: Arbitrary[FieldOptions] = Arbitrary {
    for {
      lazyf                <- Gen.option(Arbitrary.arbBool.arbitrary)
      deprecated           <- Gen.option(Arbitrary.arbBool.arbitrary)
      uninterpretedOptions <- Gen.containerOfN[Seq, UninterpretedOption](1, uninterpretedOptionSample.arbitrary)
    } yield
      new FieldOptions(
        `lazy` = lazyf,
        deprecated = deprecated,
        uninterpretedOption = uninterpretedOptions
      )
  }

  def sampleFieldDescProto(packageName: String, messageName: String): Arbitrary[FieldDescriptorProto] = Arbitrary {
    for {
      name      <- nonEmptyString
      number    <- smallNumber
      label     <- labelGenerator
      fieldType <- Gen.lzy(fieldTypeGenerator)
      options   <- Gen.lzy(Gen.option(sampleFieldOptions.arbitrary))
    } yield
      new FieldDescriptorProto(
        Some(name),
        Some(number),
        Some(label),
        Some(fieldType),
        typeName = Some(s".$packageName.$messageName"),
        extendee = None,
        defaultValue = None,
        oneofIndex = Some(1),
        Some(name),
        options
      )
  }

  // Note: there are some constraints for reserved ranges that are not currently reflected in the generators
  lazy val sampleReservedRangeProto: Arbitrary[ReservedRange] = Arbitrary {
    for {
      maybeStart <- Gen.option(smallNumber)
      maybeEnd = maybeStart.map(_ + 1)
    } yield new ReservedRange(maybeStart, maybeEnd)
  }

  lazy val enumOptions: Arbitrary[EnumOptions] = Arbitrary {
    for {
      allowAlias          <- Gen.option(sampleBool)
      deprecated          <- Gen.option(sampleBool)
      uninterpretedOption <- Gen.containerOfN[Seq, UninterpretedOption](2, uninterpretedOptionSample.arbitrary)
    } yield
      new EnumOptions(
        allowAlias,
        deprecated,
        uninterpretedOption,
        UnknownFieldSet()
      )
  }

  lazy val enumValueOptions: Arbitrary[EnumValueOptions] = Arbitrary {
    for {
      deprecated          <- Gen.option(sampleBool)
      uninterpretedOption <- Gen.containerOfN[Seq, UninterpretedOption](2, uninterpretedOptionSample.arbitrary)
    } yield new EnumValueOptions(deprecated, uninterpretedOption, UnknownFieldSet())
  }

  lazy val sampleEnumValueDescriptor: Arbitrary[EnumValueDescriptorProto] = Arbitrary {
    for {
      name           <- nonEmptyString
      number         <- smallNumber
      enumValOptions <- Gen.option(enumValueOptions.arbitrary)
    } yield
      new EnumValueDescriptorProto(
        name = Some(name),
        number = Some(number),
        options = enumValOptions
      )
  }

  lazy val sampleEnumDescriptor: Arbitrary[EnumDescriptorProto] = Arbitrary {
    for {
      name                  <- nonEmptyString
      valueDescriptorLength <- Gen.choose(1, 3)
      enumValues <- Gen.lzy(
        Gen.containerOfN[Seq, EnumValueDescriptorProto](valueDescriptorLength, sampleEnumValueDescriptor.arbitrary))
      enumOptions <- Gen.option(enumOptions.arbitrary)
    } yield
      new EnumDescriptorProto(
        name = Some(name),
        value = enumValues,
        options = enumOptions
      )
  }

  lazy val sampleOneOfDescriptor: Arbitrary[OneofDescriptorProto] = Arbitrary {
    for {
      name <- nonEmptyString
    } yield new OneofDescriptorProto(name = Some(name), options = None)
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
      enums         <- Gen.lzy(Gen.containerOfN[Seq, EnumDescriptorProto](oneOrZero, sampleEnumDescriptor.arbitrary))
      oneOfs        <- Gen.lzy(Gen.containerOfN[Seq, OneofDescriptorProto](5, sampleOneOfDescriptor.arbitrary))
      reservedRange <- Gen.lzy(Gen.containerOfN[Seq, ReservedRange](oneOrZero, sampleReservedRangeProto.arbitrary))
      reservedNames <- Gen.lzy(Gen.containerOfN[Seq, String](oneOrZero, nonEmptyString))
    } yield
      new DescriptorProto(
        name = Some(messageName),
        field = fields,
        extension = Seq(),
        nestedType = nestedTypes,
        enumType = enums,
        extensionRange = Seq(),
        oneofDecl = oneOfs, // TODO
        options = None,
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
        dependency = Seq(),
        publicDependency = Seq(),
        weakDependency = Seq(),
        messageType = messages,
        enumType = enums,
        service = Seq(), // These generators do not create services for now
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

  def protobufFMessageWithRepeatFields[T](withRepeat: Boolean)(
      implicit B: Basis[ProtobufF, T]): Gen[ProtobufF.TMessage[T]] = {

    val innerTypes: Gen[ProtobufF[T]] = Gen.oneOf(
      List(
        ProtobufF.TDouble[T](),
        ProtobufF.TFloat[T](),
        ProtobufF.TInt32[T](),
        ProtobufF.TInt64[T](),
        ProtobufF.TUint32[T](),
        ProtobufF.TUint64[T](),
        ProtobufF.TSint32[T](),
        ProtobufF.TSint64[T](),
        ProtobufF.TFixed32[T](),
        ProtobufF.TFixed64[T](),
        ProtobufF.TSfixed32[T](),
        ProtobufF.TSfixed64[T](),
        ProtobufF.TBool[T](),
        ProtobufF.TString[T](),
        ProtobufF.TBytes[T]()
      )
    )

    val sampleField: Gen[ProtobufF.Field[T]] = {
      for {
        name     <- nonEmptyString
        tpe      <- innerTypes
        position <- smallNumber
      } yield ProtobufF.Field(name, B.algebra(tpe), position, List(), withRepeat)
    }

    for {
      name  <- nonEmptyString
      field <- sampleField
    } yield ProtobufF.TMessage(name, List(field), List())
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
        sampleBool
      ).mapN((t1, t2, reversed) =>
        MuF.TCoproduct(if (reversed) NonEmptyList.of(B.algebra(t2), B.algebra(t1))
        else NonEmptyList.of(B.algebra(t1), B.algebra(t2))))
    }

  def muCoproductWithTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = true).arbitrary

  def muCoproductWithoutTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = false).arbitrary
}
