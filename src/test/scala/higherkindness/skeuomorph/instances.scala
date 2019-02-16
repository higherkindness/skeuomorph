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

package higherkindness.skeuomorph

//import java.util

import cats.data.NonEmptyList
import cats.implicits._
import org.apache.avro.Schema
import org.scalacheck._
import org.scalacheck.cats.implicits._
import mu.MuF
import avro.AvroF
import protobuf._
import qq.droste.Basis
//import com.google.protobuf.ByteString
//import com.google.protobuf.DescriptorProtos.DescriptorProto.ReservedRange
//import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Label, Type}
//import com.google.protobuf.DescriptorProtos.UninterpretedOption.NamePart
//import com.google.protobuf.DescriptorProtos._
//import com.google.protobuf.Descriptors.FileDescriptor

import scala.collection.JavaConverters._

object instances {
  lazy val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

  lazy val smallNumber: Gen[Int] = Gen.choose(1, 10)

  lazy val sampleBool: Gen[Boolean] = Gen.oneOf(true, false)

//  lazy val uninterpretedOptionNamePart: Gen[NamePart] = for {
//      name <- nonEmptyString
//      isExtension = false
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.UninterpretedOption.NamePart.newBuilder
//      javaPbOut.setNamePart(name)
//      javaPbOut.setIsExtension(isExtension)
//      javaPbOut.build
//    }
//
//  val uninterpretedOptionSample: Gen[UninterpretedOption] = for {
//      name             <- Gen.containerOfN[Seq, NamePart](2, uninterpretedOptionNamePart)
//      identifierValue  <- nonEmptyString
//      positiveIntValue <- smallNumber.map(_.toLong)
//      negativeIntValue <- smallNumber.map(_.toLong * -1)
//      doubleValue      <- Gen.oneOf(2.0, 3.0)
//      stringValue      <- nonEmptyString.map(s => ByteString.copyFrom(s.getBytes("UTF-8")))
//    } yield {
//    val javaPbOut = com.google.protobuf.DescriptorProtos.UninterpretedOption.newBuilder
//    javaPbOut.addAllName(name.asJava)
//    javaPbOut.setIdentifierValue(identifierValue)
//    javaPbOut.setPositiveIntValue(positiveIntValue)
//    javaPbOut.setNegativeIntValue(negativeIntValue)
//    javaPbOut.setDoubleValue(doubleValue)
//    javaPbOut.setStringValue(stringValue)
//    javaPbOut.build
//  }
//
//  lazy val labelGenerator: Gen[Label] = Gen.oneOf(Seq(Label.LABEL_REPEATED))
//
//  /* Type.TYPE_ENUM and Type.TYPE_MESSAGE are valid types but if added to these generators
//    they break the scalaPB parser for some reason. Type.TYPE_GROUP is not valid in Proto3
//    and therefore should not be generated. */
//  lazy val fieldTypeGenerator: Gen[FieldDescriptorProto.Type] = Gen.oneOf(
//    Seq(
//      Type.TYPE_DOUBLE,
//      Type.TYPE_FLOAT,
//      Type.TYPE_INT32,
//      Type.TYPE_INT64,
//      Type.TYPE_UINT32,
//      Type.TYPE_UINT64,
//      Type.TYPE_SINT32,
//      Type.TYPE_SINT64,
//      Type.TYPE_FIXED32,
//      Type.TYPE_FIXED64,
//      Type.TYPE_SFIXED32,
//      Type.TYPE_SFIXED64,
//      Type.TYPE_BOOL,
//      Type.TYPE_STRING,
//      Type.TYPE_BYTES
//    )
//  )
//
//  lazy val sampleFieldOptions: Gen[FieldOptions] = for {
//      lazyf                <- sampleBool
//      deprecated           <- sampleBool
//      uninterpretedOptions <- Gen.containerOfN[Seq, UninterpretedOption](1, uninterpretedOptionSample)
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.FieldOptions.newBuilder
//      javaPbOut.setLazy(lazyf)
//      javaPbOut.setDeprecated(deprecated)
//      javaPbOut.addAllUninterpretedOption(uninterpretedOptions.asJava)
//      javaPbOut.build
//    }
//
//  def sampleFieldDescProto(
//      packageName: String,
//      messageName: String,
//      oneOfIndex: Option[Int]): Gen[FieldDescriptorProto] = for {
//      name      <- nonEmptyString
//      number    <- smallNumber
//      label     <- labelGenerator
//      fieldType <- Gen.lzy(fieldTypeGenerator)
//      options   <- Gen.lzy(sampleFieldOptions)
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.FieldDescriptorProto.newBuilder
//      javaPbOut.setName(name)
//      javaPbOut.setNumber(number)
//      javaPbOut.setLabel(label)
//      javaPbOut.setType(fieldType)
//      javaPbOut.setOptions(options)
//      javaPbOut.build
//    }
//
//  lazy val sampleReservedRangeProto: Gen[ReservedRange] = for {
//      maybeStart <- Gen.option(smallNumber)
//      maybeEnd = maybeStart.map(_ + 1)
//    } yield new ReservedRange(maybeStart, maybeEnd)
//
//  lazy val enumOptions: Gen[EnumOptions] = for {
//      allowAlias          <- sampleBool
//      deprecated          <- sampleBool
//      uninterpretedOption <- Gen.containerOfN[Seq, UninterpretedOption](2, uninterpretedOptionSample)
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.EnumOptions.newBuilder
//      javaPbOut.setAllowAlias(allowAlias)
//      javaPbOut.setDeprecated(deprecated)
//      javaPbOut.addAllUninterpretedOption(uninterpretedOption.asJava)
//      javaPbOut.build
//    }
//
//  lazy val enumValueOptions: Arbitrary[EnumValueOptions] = Arbitrary {
//    for {
//      deprecated          <- Gen.option(sampleBool)
//      uninterpretedOption <- Gen.containerOfN[Seq, UninterpretedOption](2, uninterpretedOptionSample)
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.EnumValueOptions.newBuilder
//      deprecated.foreach(javaPbOut.setDeprecated)
//      javaPbOut.addAllUninterpretedOption(uninterpretedOption.asJava)
//      javaPbOut.build
//    }
//  }
//
//  def sampleEnumValueDescriptor: Gen[EnumValueDescriptorProto] = for {
//      name           <- nonEmptyString
//      enumValOptions <- enumValueOptions.arbitrary
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.EnumValueDescriptorProto.newBuilder
//      javaPbOut.setName(name)
//      javaPbOut.setOptions(enumValOptions)
//      javaPbOut.build
//    }
//
//  private def setNumber(value: EnumValueDescriptorProto, number: Int): EnumValueDescriptorProto = {
//    val javaPbOut = com.google.protobuf.DescriptorProtos.EnumValueDescriptorProto.newBuilder
//    javaPbOut.setName(value.getName)
//    javaPbOut.setOptions(value.getOptions)
//    javaPbOut.setNumber(number)
//    javaPbOut.build
//  }
//
//  lazy val sampleEnumDescriptor: Gen[EnumDescriptorProto] = for {
//      name                  <- nonEmptyString
//      valueDescriptorLength <- Gen.choose(1, 3)
//      enumValues <- Gen.lzy(Gen.containerOfN[Seq, EnumValueDescriptorProto](valueDescriptorLength, sampleEnumValueDescriptor))
//      enumValuesWithNumber = enumValues.zipWithIndex.map { case (enum, i) => setNumber(enum, i) }
//      enumOptions <- Gen.option(enumOptions)
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.EnumDescriptorProto.newBuilder
//      javaPbOut.setName(name)
//      javaPbOut.addAllValue(enumValuesWithNumber.asJava)
//      enumOptions.foreach(javaPbOut.setOptions)
//      javaPbOut.build
//    }
//
//  lazy val sampleOneOfDescriptor: Gen[OneofDescriptorProto] = nonEmptyString.map { name =>
//      val javaPbOut = com.google.protobuf.DescriptorProtos.OneofDescriptorProto.newBuilder
//      javaPbOut.setName(name)
//      javaPbOut.build
//    }
//
//  def sampleDescriptorProto(packageName: String): Gen[DescriptorProto] = for {
//      name      <- nonEmptyString
//      oneOrZero <- Gen.choose(0, 1)
//      oneOfs    <- Gen.lzy(Gen.containerOfN[Seq, OneofDescriptorProto](2, sampleOneOfDescriptor))
//      fields <- Gen.lzy(
//        Gen.containerOfN[Seq, FieldDescriptorProto](10, sampleFieldDescProto(packageName, name, None)))
//      oneOfFields <- Gen.sequence[List[FieldDescriptorProto], FieldDescriptorProto](
//        oneOfs.zipWithIndex
//          .map { case (oneOf, i) => sampleFieldDescProto(packageName, oneOf.getName, Some(i)) }
//      )
//      nestedTypes <- Gen.lzy(
//        Gen.containerOfN[Seq, DescriptorProto](oneOrZero, sampleDescriptorProto(packageName)))
//      enums         <- Gen.lzy(Gen.containerOfN[Seq, EnumDescriptorProto](oneOrZero, sampleEnumDescriptor))
//      reservedRange <- Gen.lzy(Gen.containerOfN[Seq, ReservedRange](oneOrZero, sampleReservedRangeProto))
//      reservedNames <- Gen.lzy(Gen.containerOfN[Seq, String](oneOrZero, nonEmptyString))
//    } yield {
//      val javaPbOut = com.google.protobuf.DescriptorProtos.DescriptorProto.newBuilder
//      javaPbOut.setName(name)
//      javaPbOut.addAllField(fields.asJava)
//      javaPbOut.addAllNestedType(nestedTypes.asJava)
//      javaPbOut.addAllEnumType(enums.asJava)
//      javaPbOut.addAllOneofDecl(oneOfFields.asJava)
//      javaPbOut.addAllReservedRange(reservedRange.asJava)
//      javaPbOut.addAllReservedName(reservedNames.asJava)
//      javaPbOut.build
//    }
//
//  lazy val sampleFileDescriptorProto: Gen[FileDescriptorProto] = for {
//      name                 <- nonEmptyString
//      packageN             <- nonEmptyString
//      messageAndEnumLength <- Gen.choose(1, 5)
//      messages <- Gen.lzy(
//        Gen.containerOfN[Seq, DescriptorProto](messageAndEnumLength, sampleDescriptorProto(packageN)))
//      enums <- Gen.lzy(Gen.containerOfN[Seq, EnumDescriptorProto](messageAndEnumLength, sampleEnumDescriptor))
//    } yield {
//    val javaPbOut = com.google.protobuf.DescriptorProtos.FileDescriptorProto.newBuilder
//    javaPbOut.setName(name)
//    javaPbOut.setPackage(packageN)
//    javaPbOut.addAllMessageType(messages.asJava)
//    javaPbOut.addAllEnumType(enums.asJava)
//    javaPbOut.build
//  }
//
//  implicit lazy val baseDescriptorArbitrary: Arbitrary[FileDescriptorProto] = Arbitrary(sampleFileDescriptorProto)

//  implicit lazy val baseDescriptorArbitrary: Arbitrary[FileDescriptor] = Arbitrary {
//    for {
//      sampleFileDescriptorProto <- Gen.lzy(sampleFileDescriptorProto)
//    } yield FileDescriptor.buildFrom(sampleFileDescriptorProto, Nil)
//  }

//  lazy val sampleNativeOneOfField: Gen[NativeOneOfField] = for {
//    name <- nonEmptyString
//    tpe  <- sampleNativeOneOf
//  } yield NativeOneOfField(name, tpe)
//
//  lazy val sampleNativeOneOf: Gen[NativeOneOf] = for {
//    name <- nonEmptyString
//    tpes <- Gen.listOfN(3, sampleNativeField)
//  } yield NativeOneOf(name, NonEmptyList.fromListUnsafe(tpes))
//
//  lazy val sampleNativeField: Gen[NativeField] =
//    for {
//      name       <- nonEmptyString
//      tpe        <- fieldTypeGenerator
//      position   <- smallNumber
//      allowAlias <- sampleBool
//      deprecated <- sampleBool
//      options = List(NativeOption("allow_alias", allowAlias.toString), NativeOption("deprecated", deprecated.toString))
//      isRepeated <- sampleBool
//      isMap      <- sampleBool
//    } yield NativeField(name, tpe, position, options, isRepeated, isMap)
//
//  lazy val fieldTypeGenerator: Gen[NativeDescriptor] = Gen.oneOf(
//    Seq(
//      NativeDouble(),
//      NativeFloat(),
//      NativeInt32(),
//      NativeInt64(),
//      NativeUint32(),
//      NativeUint64(),
//      NativeSint32(),
//      NativeSint64(),
//      NativeFixed32(),
//      NativeFixed64(),
//      NativeSfixed32(),
//      NativeSfixed64(),
//      NativeBool(),
//      NativeString(),
//      NativeBytes()
//    ))
//
//  lazy val sampleNativeEnum: Gen[NativeEnum] = for {
//    name                  <- nonEmptyString
//    valueDescriptorLength <- Gen.choose(1, 3)
//    enumValues            <- Gen.lzy(Gen.containerOfN[List, String](valueDescriptorLength, nonEmptyString))
//    symbols = enumValues.zipWithIndex
//    enumAliases <- Gen.lzy(Gen.containerOfN[List, String](valueDescriptorLength, nonEmptyString))
//    aliases = enumAliases.zipWithIndex
//    allowAlias <- sampleBool
//    deprecated <- sampleBool
//    options = List(NativeOption("allow_alias", allowAlias.toString), NativeOption("deprecated", deprecated.toString))
//  } yield
//    NativeEnum(
//      name = name,
//      symbols = symbols,
//      options = options,
//      aliases = aliases
//    )
//
//  def sampleNativeMessage(packageName: String): Gen[NativeMessage] =
//    for {
//      name        <- nonEmptyString
//      oneOrZero   <- Gen.choose(0, 1)
//      fields      <- Gen.listOfN(2, sampleNativeField)
//      oneOfFields <- Gen.listOfN(1, sampleNativeOneOfField)
//      reserved    <- smallNumber
//      ranges = List(List(reserved, reserved + 1).map(_.toString), List(reserved + 10, reserved + 12).map(_.toString))
//      nestedTypes <- Gen.lzy(Gen.containerOfN[List, NativeMessage](oneOrZero, sampleNativeMessage(packageName)))
//    } yield NativeMessage(name = name, fields = fields ++ oneOfFields, reserved = ranges, nested = nestedTypes)
//
//  def sampleNativeOperation(messages: List[NativeDescriptor]): Gen[NativeOperation] =
//    for {
//      name              <- nonEmptyString
//      request           <- Gen.oneOf(messages)
//      requestStreaming  <- sampleBool
//      response          <- Gen.oneOf(messages)
//      responseStreaming <- sampleBool
//    } yield NativeOperation(name, request, requestStreaming, response, responseStreaming)
//
//  def sampleNativeService(messages: List[NativeDescriptor]): Gen[NativeService] =
//    for {
//      name       <- nonEmptyString
//      operations <- Gen.listOfN(2, sampleNativeOperation(messages))
//    } yield NativeService(name, operations)
//
//  lazy val nativeFileGen: Gen[NativeFile] = for {
//    name                 <- nonEmptyString
//    packageN             <- nonEmptyString
//    messageAndEnumLength <- Gen.choose(1, 5)
//    messages             <- Gen.lzy(Gen.containerOfN[List, NativeDescriptor](messageAndEnumLength, sampleNativeMessage(packageN)))
//    enums                <- Gen.lzy(Gen.containerOfN[List, NativeDescriptor](messageAndEnumLength, sampleNativeEnum))
//    services             <- Gen.lzy(Gen.containerOfN[List, NativeService](messageAndEnumLength, sampleNativeService(messages)))
//  } yield NativeFile(name = name, `package` = packageN, values = messages ++ enums, services = services)
//
//  implicit val nativeFileArb: Arbitrary[NativeFile] = Arbitrary(nativeFileGen)

  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////

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

    val sampleField: Gen[FieldF.Field[T]] = {
      for {
        name     <- nonEmptyString
        tpe      <- innerTypes
        position <- smallNumber
      } yield FieldF.Field(name, B.algebra(tpe), position, List(), withRepeat, isMapField = false)
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

  implicit def muArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[MuF[T]] = {

    def fieldGen: Gen[MuF.Field[T]] =
      (
        nonEmptyString,
        Gen.lzy(T.arbitrary)
      ).mapN(MuF.Field.apply)

    Arbitrary(
      Gen.oneOf(
        MuF.`null`[T]().pure[Gen],
        MuF.double[T]().pure[Gen],
        MuF.float[T]().pure[Gen],
        MuF.int[T]().pure[Gen],
        MuF.long[T]().pure[Gen],
        MuF.boolean[T]().pure[Gen],
        MuF.string[T]().pure[Gen],
        MuF.byteArray[T]().pure[Gen],
        nonEmptyString map MuF.namedType[T],
        T.arbitrary map MuF.option[T],
        (T.arbitrary, T.arbitrary) mapN { (a, b) =>
          MuF.either(a, b)
        },
        T.arbitrary map MuF.list[T],
        T.arbitrary map (t => MuF.map[T](None, t)),
        T.arbitrary map MuF.required[T],
        (T.arbitrary, Gen.listOf(T.arbitrary)) mapN { (a, b) =>
          MuF.generic[T](a, b)
        },
        (T.arbitrary, Gen.listOf(T.arbitrary)) mapN { (a, b) =>
          MuF.generic[T](a, b)
        },
        Gen.nonEmptyListOf(T.arbitrary) map { l =>
          MuF.coproduct[T](NonEmptyList.fromListUnsafe(l))
        },
        (nonEmptyString, Gen.nonEmptyListOf(Gen.lzy(fieldGen))).mapN { (n, f) =>
          MuF.product(n, f)
        }
      ))
  }

  implicit def avroArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[AvroF[T]] = {

    val orderGen: Gen[AvroF.Order] = Gen.oneOf(AvroF.Order.Ascending, AvroF.Order.Descending, AvroF.Order.Ignore)

    val fieldGen: Gen[AvroF.Field[T]] = (
      nonEmptyString,
      Gen.listOf(nonEmptyString),
      Gen.option(nonEmptyString),
      Gen.option(orderGen),
      T.arbitrary
    ).mapN(AvroF.Field.apply[T])

    Arbitrary(
      Gen.oneOf(
        AvroF.`null`[T]().pure[Gen],
        AvroF.boolean[T]().pure[Gen],
        AvroF.int[T]().pure[Gen],
        AvroF.long[T]().pure[Gen],
        AvroF.float[T]().pure[Gen],
        AvroF.double[T]().pure[Gen],
        AvroF.bytes[T]().pure[Gen],
        AvroF.string[T]().pure[Gen],
        nonEmptyString map AvroF.namedType[T],
        T.arbitrary map AvroF.array[T],
        T.arbitrary map AvroF.map[T],
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.option(nonEmptyString),
          Gen.listOf(fieldGen)
        ).mapN(AvroF.record[T]),
        Gen.nonEmptyListOf(T.arbitrary) map { l =>
          AvroF.union[T](NonEmptyList.fromListUnsafe(l))
        },
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString)
        ).mapN(AvroF.enum[T]),
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.posNum[Int]
        ).mapN(AvroF.fixed[T])
      ))
  }

  implicit def protoArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[ProtobufF[T]] = {
    val genOption: Gen[ProtobufF.OptionValue] = (nonEmptyString, nonEmptyString).mapN(ProtobufF.OptionValue.apply)
    Arbitrary(
      Gen.oneOf(
        ProtobufF.double[T]().pure[Gen],
        ProtobufF.float[T]().pure[Gen],
        ProtobufF.int32[T]().pure[Gen],
        ProtobufF.int64[T]().pure[Gen],
        ProtobufF.uint32[T]().pure[Gen],
        ProtobufF.uint64[T]().pure[Gen],
        ProtobufF.sint32[T]().pure[Gen],
        ProtobufF.sint64[T]().pure[Gen],
        ProtobufF.fixed32[T]().pure[Gen],
        ProtobufF.fixed64[T]().pure[Gen],
        ProtobufF.sfixed32[T]().pure[Gen],
        ProtobufF.sfixed64[T]().pure[Gen],
        ProtobufF.bool[T]().pure[Gen],
        ProtobufF.string[T]().pure[Gen],
        ProtobufF.bytes[T]().pure[Gen],
        nonEmptyString map ProtobufF.namedType[T],
        T.arbitrary map ProtobufF.repeated[T],
        (
          nonEmptyString,
          Gen.listOf((nonEmptyString, Gen.posNum[Int]).tupled),
          Gen.listOf(genOption),
          Gen.listOf((nonEmptyString, Gen.posNum[Int]).tupled)
        ).mapN(ProtobufF.enum[T]),
        (
          nonEmptyString,
          Gen.listOf(
            (
              nonEmptyString,
              T.arbitrary,
              Gen.posNum[Int],
              Gen.listOf(genOption),
              sampleBool,
              sampleBool
            ).mapN(FieldF.Field.apply[T])
          ),
          Gen.listOf(Gen.listOf(nonEmptyString))
        ).mapN(ProtobufF.message[T])
      )
    )
  }

  def muCoproductWithTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = true).arbitrary

  def muCoproductWithoutTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = false).arbitrary
}
