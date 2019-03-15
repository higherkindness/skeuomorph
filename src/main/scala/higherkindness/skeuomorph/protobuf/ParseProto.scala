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

package higherkindness.skeuomorph.protobuf

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.os72.protocjar.Protoc
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Label, Type => ProtoType}
import com.google.protobuf.DescriptorProtos.FileDescriptorSet
import com.google.protobuf.DescriptorProtos.UninterpretedOption.NamePart
import com.google.protobuf.DescriptorProtos._
import higherkindness.skeuomorph.compdata.Ann
import higherkindness.skeuomorph.uast.{types => t}
import higherkindness.skeuomorph.uast.derivation._
import higherkindness.skeuomorph.FileUtils._
import higherkindness.skeuomorph.{Parser, _}
import java.util

import qq.droste._
import qq.droste.syntax.embed._

import scala.collection.JavaConverters._

object ParseProto {

  import Protocol._

  case class ProtoSource(filename: String, path: String)

  implicit def parseProto[F[_], T](implicit T: Embed[protobuf.Type, T]): Parser[F, ProtoSource, Protocol[T]] =
    new Parser[F, ProtoSource, Protocol[T]] {
      override def parse(input: ProtoSource)(implicit S: Sync[F]): F[Protocol[T]] =
        runProtoc(input)
    }

  private def runProtoc[F[_]: Sync, T](input: ProtoSource)(implicit T: Embed[protobuf.Type, T]): F[Protocol[T]] = {
    val descriptorFileName = s"${input.filename}.desc"
    val protoCompilation: F[Int] = Sync[F].delay(
      Protoc.runProtoc(
        Array(
          "--plugin=protoc-gen-proto2_to_proto3",
          "--include_imports",
          s"--descriptor_set_out=${input.filename}.desc",
          s"--proto_path=${input.path}",
          input.filename
        )
      )
    )

    for {
      _ <- Sync[F].ensure[Int](protoCompilation)(ProtobufCompilationException())((exitCode: Int) => exitCode == 0)
      fileDescriptor <- Sync[F].adaptError(makeFileDescriptor[F](descriptorFileName)) {
        case ex: Exception => ProtobufParsingException(ex)
      }
      nativeDescriptors <- getTFiles[F, T](input.filename, fileDescriptor)
    } yield nativeDescriptors
  }

  private def makeFileDescriptor[F[_]: Sync](descriptorFileName: String): F[FileDescriptorSet] =
    fileInputStream(descriptorFileName).use(fis => Sync[F].delay(FileDescriptorSet.parseFrom(fis)))

  private def getTFiles[F[_]: Sync, T](descriptorFileName: String, source: FileDescriptorSet)(
      implicit T: Embed[protobuf.Type, T]
  ): F[Protocol[T]] = {
    Sync[F].delay {
      fromProto[T](descriptorFileName, source.getFileList.asScala.toList)
    }
  }

  def fromProto[A](
      descriptorFileName: String,
      files: List[FileDescriptorProto]
  )(implicit A: Embed[protobuf.Type, A]): Protocol[A] =
    findDescriptorProto(descriptorFileName, files)
      .map { file =>
        val dependents: List[A] = file.getDependencyList.j2s
          .flatMap(findDescriptorProto(_, files))
          .flatMap(getDependentValues(_, files))

        val messages: List[A] = file.getMessageTypeList.j2s.map(d => toMessage[A](d, files))

        val enums: List[A] = file.getEnumTypeList.j2s.map(toEnum[A])

        Protocol[A](
          formatName(file.getName),
          file.getPackage,
          Nil,
          dependents ++ messages ++ enums,
          file.getServiceList.j2s.map(s => toService[A](s, files))
        )
      }
      .getOrElse(throw ProtobufNativeException(s"Could not find descriptors for: $descriptorFileName"))

  def findDescriptorProto(name: String, files: List[FileDescriptorProto]): Option[FileDescriptorProto] =
    files.find(_.getName == name)

  def getDependentValues[A](dependent: FileDescriptorProto, files: List[FileDescriptorProto])(
      implicit A: Embed[protobuf.Type, A]): List[A] =
    dependent.getMessageTypeList.j2s.map(d => toMessage(d, files)) ++
      dependent.getEnumTypeList.j2s.map(toEnum(_)(A))

  def formatName(name: String): String = name.replace(".proto", "")

  def toService[A](s: ServiceDescriptorProto, files: List[FileDescriptorProto])(
      implicit A: Embed[protobuf.Type, A]): Service[A] =
    Service(s.getName, s.getMethodList.j2s.map(o => toOperation[A](o, files)))

  def toOperation[A](o: MethodDescriptorProto, files: List[FileDescriptorProto])(
      implicit A: Embed[Type, A]): Protocol.Operation[A] =
    Protocol.Operation(
      name = o.getName,
      request = findMessage(o.getInputType, files)
        .map(msg => toMessage(msg, files))
        .getOrElse(t.`null`[protobuf.Type, A].embed),
      requestStreaming = o.getClientStreaming,
      response = findMessage(o.getOutputType, files)
        .map(msg => toMessage(msg, files))
        .getOrElse(t.`null`[protobuf.Type, A].embed),
      responseStreaming = o.getServerStreaming
    )

  def toMessage[A](descriptor: DescriptorProto, files: List[FileDescriptorProto])(
      implicit A: Embed[protobuf.Type, A]): A = {
    val protoFields: List[FieldDescriptorProto] = descriptor.getFieldList.j2s
    val protoOneOf: List[OneofDescriptorProto]  = descriptor.getOneofDeclList.j2s
    val oneOfFields: List[(t.FieldF[A], List[Int])] =
      fromOneofDescriptorsProto(protoOneOf, protoFields, descriptor, files)
    val oneOfNumbers: List[Int] = oneOfFields.flatMap(_._2)
    val fields: List[t.FieldF[A]] =
      protoFields
        .filterNot(f => oneOfNumbers.contains(f.getNumber))
        .map(f => fromFieldDescriptorProto[A](f, descriptor, files))

    message[protobuf.Type, A](
      name = descriptor.getName,
      fields = fields ++ oneOfFields.map(_._1),
      reserved =
        descriptor.getReservedRangeList.j2s.map(range => (range.getStart until range.getEnd).map(_.toString).toList)
    ).embed
  }

  def toEnum[A](enum: EnumDescriptorProto)(implicit A: Embed[protobuf.Type, A]): A = {
    val (values, aliases) = partitionValuesAliases(enum.getValueList.j2s)

    A.algebra(
      protoEnum[protobuf.Type, A](
        name = enum.getName,
        symbols = values,
        options = fromFieldOptionsEnum(enum.getOptions),
        aliases = aliases))
  }

  def partitionValuesAliases(
      valuesAndAliases: List[EnumValueDescriptorProto]): (List[(String, Int)], List[(String, Int)]) = {
    val (hasAlias, noAlias) = valuesAndAliases.groupBy(_.getNumber).values.partition(_.lengthCompare(1) > 0)
    val separateValueFromAliases: Iterable[(EnumValueDescriptorProto, List[EnumValueDescriptorProto])] = hasAlias.map {
      case h :: t => (h, t)
      case _      => throw ProtobufNativeException(s"Wrong number of aliases")
    }

    (
      (separateValueFromAliases.map(_._1) ++ noAlias.flatten).toList
        .sortBy(_.getNumber)
        .map(i => (i.getName, i.getNumber)),
      separateValueFromAliases.flatMap(_._2).toList.map(i => (i.getName, i.getNumber)))
  }

  def fromFieldDescriptorProto[A](
      field: FieldDescriptorProto,
      source: DescriptorProto,
      files: List[FileDescriptorProto])(implicit A: Embed[protobuf.Type, A]): t.FieldF[A] =
    (field.getLabel.isRepeated, isMap(field, source)) match {
      case (true, false) =>
        t.FieldF.ProtobufField(
          name = field.getName,
          tpe = t.list[protobuf.Type, A](fromFieldType(field, files)).embed,
          position = field.getNumber,
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = true,
          isMapField = false
        )
      case (_, true) =>
        t.FieldF.ProtobufField(
          name = field.getName,
          position = field.getNumber,
          tpe = getTMap(field.getName, source, files),
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = false,
          isMapField = true
        )
      case _ =>
        t.FieldF.ProtobufField(
          name = field.getName,
          position = field.getNumber,
          tpe = fromFieldType(field, files),
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = field.getLabel.isRepeated,
          isMapField = isMap(field, source)
        )
    }

  def isMap(field: FieldDescriptorProto, source: DescriptorProto): Boolean =
    source.getNestedTypeList.j2s.exists(
      e =>
        e.getOptions.getMapEntry &&
          matchNameEntry(field.getName, e) &&
          takeOnlyMapEntries(e.getFieldList.j2s))

  def getTMap[A](name: String, source: DescriptorProto, files: List[FileDescriptorProto])(
      implicit A: Embed[protobuf.Type, A]): A =
    (for {
      maybeMsg <- source.getNestedTypeList.j2s
        .find(e => e.getOptions.getMapEntry && matchNameEntry(name, e) && takeOnlyMapEntries(e.getFieldList.j2s))
      maybeKey   <- getMapField(maybeMsg, "key")
      maybeValue <- getMapField(maybeMsg, "value")
    } yield t.map[Type, A](fromFieldType(maybeKey, files), fromFieldType(maybeValue, files)).embed)
      .getOrElse(throw ProtobufNativeException(s"Could not find map entry for: $name"))

  def getMapField(msg: DescriptorProto, name: String): Option[FieldDescriptorProto] =
    msg.getFieldList.j2s.find(_.getName == name)

  def takeOnlyMapEntries(fields: List[FieldDescriptorProto]): Boolean =
    fields.count(f => (f.getNumber == 1 && f.getName == "key") || (f.getNumber == 2 && f.getName == "value")) == 2

  def matchNameEntry(name: String, source: DescriptorProto): Boolean =
    source.getName.toLowerCase == s"${name}Entry".toLowerCase

  def fromOneofDescriptorsProto[A](
      oneOfFields: List[OneofDescriptorProto],
      fields: List[FieldDescriptorProto],
      source: DescriptorProto,
      files: List[FileDescriptorProto])(
      implicit A: Embed[protobuf.Type, A]
  ): List[(t.FieldF[A], List[Int])] = oneOfFields.zipWithIndex.map {
    case (oneof, index) => {
      val oneOfFields: NonEmptyList[t.FieldF[A]] = NonEmptyList
        .fromList(
          fields
            .filter(t => t.hasOneofIndex && t.getOneofIndex == index)
            .map(fromFieldDescriptorProto(_, source, files))
            .collect { case b @ t.FieldF.InjProtobufField(_) => b })
        .getOrElse(throw ProtobufNativeException(s"Empty set of fields in OneOf: ${oneof.getName}"))

      val fOneOf    = oneOf[protobuf.Type, A](name = oneof.getName, fields = oneOfFields)
      val positions = oneOfFields.map({ case t.FieldF.InjProtobufField(Ann(_, (pos, _, _, _))) => pos }).toList
      (t.FieldF.SimpleField(name = oneof.getName, tpe = fOneOf.embed), positions)
    }
  }

  def fromFieldTypeCoalgebra(
      field: FieldDescriptorProto,
      files: List[FileDescriptorProto]
  ): Coalgebra[Type, ProtoType] = Coalgebra {
    case ProtoType.TYPE_BOOL     => t.boolean[Type, ProtoType]
    case ProtoType.TYPE_BYTES    => t.byteArray[Type, ProtoType]
    case ProtoType.TYPE_DOUBLE   => t.double[Type, ProtoType]
    case ProtoType.TYPE_FIXED32  => fixed32[Type, ProtoType]
    case ProtoType.TYPE_FIXED64  => fixed64[Type, ProtoType]
    case ProtoType.TYPE_FLOAT    => t.float[Type, ProtoType]
    case ProtoType.TYPE_INT32    => int32[Type, ProtoType]
    case ProtoType.TYPE_INT64    => int64[Type, ProtoType]
    case ProtoType.TYPE_SFIXED32 => fixed32[Type, ProtoType]
    case ProtoType.TYPE_SFIXED64 => fixed64[Type, ProtoType]
    case ProtoType.TYPE_SINT32   => int32[Type, ProtoType]
    case ProtoType.TYPE_SINT64   => int64[Type, ProtoType]
    case ProtoType.TYPE_STRING   => t.string[Type, ProtoType]
    case ProtoType.TYPE_UINT32   => int32[Type, ProtoType]
    case ProtoType.TYPE_UINT64   => int64[Type, ProtoType]
    case ProtoType.TYPE_ENUM =>
      findEnum(field.getTypeName, files)
        .fold[protobuf.Type[ProtoType]](t.`null`[Type, ProtoType])(e => t.namedType[Type, ProtoType](e.getName))
    case ProtoType.TYPE_MESSAGE =>
      findMessage(field.getTypeName, files)
        .fold[protobuf.Type[ProtoType]](t.`null`[Type, ProtoType])(e => t.namedType[Type, ProtoType](e.getName))
    case _ => t.`null`[Type, ProtoType]
  }

  def fromFieldType[A](field: FieldDescriptorProto, files: List[FileDescriptorProto])(
      implicit A: Embed[protobuf.Type, A]): A =
    scheme.ana(fromFieldTypeCoalgebra(field, files)).apply(field.getType)

  def fromFieldOptionsMsg(options: FieldOptions): List[OptionValue] =
    OptionValue("deprecated", options.getDeprecated.toString) ::
      options.getUninterpretedOptionList.j2s.map(t => OptionValue(toString(t.getNameList.j2s), t.getIdentifierValue))

  def fromFieldOptionsEnum(options: EnumOptions): List[OptionValue] = {
    List(
      OptionValue("allow_alias", options.getAllowAlias.toString),
      OptionValue("deprecated", options.getDeprecated.toString)) ++
      options.getUninterpretedOptionList.j2s.map(t => OptionValue(toString(t.getNameList.j2s), t.getIdentifierValue))
  }

  def toString(nameParts: Seq[NamePart]): String =
    nameParts.foldLeft("")((l, r) => if (r.getIsExtension) s"$l.($r)" else s"$l.$r")

  def findMessage(name: String, files: List[FileDescriptorProto]): Option[DescriptorProto] = {
    case class NamedMessage(fullName: String, msg: DescriptorProto)
    val all: List[NamedMessage] = files.flatMap(f =>
      f.getMessageTypeList.j2s.flatMap(m =>
        NamedMessage(s".${f.getPackage}.${m.getName}", m) :: m.getNestedTypeList.j2s.map(n =>
          NamedMessage(s".${f.getPackage}.${m.getName}.${n.getName}", n))))
    all.find(_.fullName == name).map(_.msg)
  }

  def findEnum(name: String, files: List[FileDescriptorProto]): Option[EnumDescriptorProto] = {
    case class NamedEnum(fullName: String, msg: EnumDescriptorProto)
    files
      .flatMap(f => f.getEnumTypeList.j2s.map(m => NamedEnum(s".${f.getPackage}.${m.getName}", m)))
      .find(_.fullName == name)
      .map(_.msg)
  }

  implicit class LabelOps(self: Label) {
    def isRepeated: Boolean = self.name() == "LABEL_REPEATED"
  }

  implicit class JavaListOps[B](self: util.List[B]) {
    def j2s: List[B] = self.asScala.toList
  }
}
