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

import java.util
import cats.data.NonEmptyList
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Label, Type}
import com.google.protobuf.DescriptorProtos.UninterpretedOption.NamePart
import com.google.protobuf.DescriptorProtos._

import scala.collection.JavaConverters._

sealed trait NativeFieldF {
  val name: String
  val tpe: NativeDescriptor
}

final case class NativeField(
    name: String,
    tpe: NativeDescriptor,
    position: Int,
    options: List[NativeOption],
    isRepeated: Boolean,
    isMapField: Boolean)
    extends NativeFieldF

final case class NativeOneOfField(name: String, tpe: NativeOneOf) extends NativeFieldF

sealed trait NativeDescriptor

final case class NativeNull() extends NativeDescriptor

final case class NativeDouble() extends NativeDescriptor

final case class NativeFloat() extends NativeDescriptor

final case class NativeInt32() extends NativeDescriptor

final case class NativeInt64() extends NativeDescriptor

final case class NativeUint32() extends NativeDescriptor

final case class NativeUint64() extends NativeDescriptor

final case class NativeSint32() extends NativeDescriptor

final case class NativeSint64() extends NativeDescriptor

final case class NativeFixed32() extends NativeDescriptor

final case class NativeFixed64() extends NativeDescriptor

final case class NativeSfixed32() extends NativeDescriptor

final case class NativeSfixed64() extends NativeDescriptor

final case class NativeBool() extends NativeDescriptor

final case class NativeString() extends NativeDescriptor

final case class NativeBytes() extends NativeDescriptor

final case class NativeNamedType(name: String) extends NativeDescriptor

final case class NativeRepeated(value: NativeDescriptor) extends NativeDescriptor

final case class NativeOneOf(name: String, fields: NonEmptyList[NativeField]) extends NativeDescriptor

final case class NativeMap(keyTpe: NativeDescriptor, value: NativeDescriptor) extends NativeDescriptor

final case class NativeEnum(
    name: String,
    symbols: List[(String, Int)],
    options: List[NativeOption],
    aliases: List[(String, Int)])
    extends NativeDescriptor

final case class NativeMessage(
    name: String,
    fields: List[NativeFieldF],
    reserved: List[List[String]],
    nested: List[NativeDescriptor])
    extends NativeDescriptor

final case class NativeFile(values: List[NativeDescriptor], name: String, `package`: String) extends NativeDescriptor

final case class NativeOption(name: String, value: String)

object NativeDescriptor {

  def apply(file: FileDescriptorProto, files: List[FileDescriptorProto]): NativeDescriptor =
    NativeFile(
      file.getMessageTypeList.j2s.map(d => toNativeMessage(d, files)) ++
        file.getEnumTypeList.j2s.map(toNativeEnum),
      file.getName,
      file.getPackage)

  def toNativeMessage(descriptor: DescriptorProto, files: List[FileDescriptorProto]): NativeDescriptor = {
    val protoFields: List[FieldDescriptorProto] = descriptor.getFieldList.j2s
    val protoOneOf: List[OneofDescriptorProto]  = descriptor.getOneofDeclList.j2s
    val oneOfFields: List[NativeFieldF] =
      fromOneofDescriptorsProto(protoOneOf, protoFields, descriptor, files)
    val oneOfNumbers: List[Int] =
      oneOfFields.map(_.tpe).collect { case o: NativeOneOf => o.fields.toList.map(_.position) }.flatten
    val fields: List[NativeFieldF] =
      protoFields
        .filterNot(f => oneOfNumbers.contains(f.getNumber))
        .map(f => fromFieldDescriptorProto(f, descriptor, files))

    NativeMessage(
      name = descriptor.getName,
      fields = fields ++ oneOfFields,
      reserved =
        descriptor.getReservedRangeList.j2s.map(range => (range.getStart until range.getEnd).map(_.toString).toList),
      nested = descriptor.getNestedTypeList.j2s.map(d => toNativeMessage(d, files))
    )
  }

  def toNativeEnum(enum: EnumDescriptorProto): NativeDescriptor = {
    val (values, aliases) = partitionValuesAliases(enum.getValueList.j2s)

    NativeEnum(
      name = enum.getName,
      symbols = values,
      options = fromFieldOptionsEnum(enum.getOptions),
      aliases = aliases)
  }

  def partitionValuesAliases(
      valuesAndAliases: List[EnumValueDescriptorProto]): (List[(String, Int)], List[(String, Int)]) = {
    val (hasAlias, noAlias)      = valuesAndAliases.groupBy(_.getNumber).values.partition(_.lengthCompare(1) > 0)
    val separateValueFromAliases = hasAlias.map(list => (list.head, list.tail))
    (
      (separateValueFromAliases.map(_._1) ++ noAlias.flatten).toList
        .sortBy(_.getNumber)
        .map(i => (i.getName, i.getNumber)),
      separateValueFromAliases.flatMap(_._2).toList.map(i => (i.getName, i.getNumber)))
  }

  def fromFieldDescriptorProto(
      field: FieldDescriptorProto,
      source: DescriptorProto,
      files: List[FileDescriptorProto]): NativeFieldF =
    (field.getLabel.isRepeated, isMap(field, source)) match {
      case (true, false) =>
        NativeField(
          name = field.getName,
          position = field.getNumber,
          tpe = NativeRepeated(fromFieldType(field, files)),
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = true,
          isMapField = false
        )
      case (_, true) =>
        NativeField(
          name = field.getName,
          position = field.getNumber,
          tpe = getNativeMap(field.getName, source, files),
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = false,
          isMapField = true
        )
      case _ =>
        NativeField(
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

  def getNativeMap(name: String, source: DescriptorProto, files: List[FileDescriptorProto]): NativeMap =
    (for {
      maybeMsg <- source.getNestedTypeList.j2s
        .find(e => e.getOptions.getMapEntry && matchNameEntry(name, e) && takeOnlyMapEntries(e.getFieldList.j2s))
      maybeKey   <- getMapField(maybeMsg, "key")
      maybeValue <- getMapField(maybeMsg, "value")
    } yield NativeMap(fromFieldType(maybeKey, files), fromFieldType(maybeValue, files)))
      .getOrElse(throw new Exception(s"Could not find map entry for: $name"))

  def getMapField(msg: DescriptorProto, name: String): Option[FieldDescriptorProto] =
    msg.getFieldList.j2s.find(_.getName == name)

  def takeOnlyMapEntries(fields: List[FieldDescriptorProto]): Boolean =
    fields.count(f => (f.getNumber == 1 && f.getName == "key") || (f.getNumber == 2 && f.getName == "value")) == 2

  def matchNameEntry(name: String, source: DescriptorProto): Boolean =
    source.getName.toLowerCase == s"${name}Entry".toLowerCase

  def fromOneofDescriptorsProto(
      oneOfFields: List[OneofDescriptorProto],
      fields: List[FieldDescriptorProto],
      source: DescriptorProto,
      files: List[FileDescriptorProto]): List[NativeFieldF] = oneOfFields.zipWithIndex.map {
    case (oneof, index) => {
      val nativeFields: List[NativeField] =
        fields
          .filter(t => t.hasOneofIndex && t.getOneofIndex == index)
          .map(fromFieldDescriptorProto(_, source, files))
          .collect { case b: NativeField => b }

      NativeOneOfField(
        name = oneof.getName,
        tpe = NativeOneOf(name = oneof.getName, fields = NonEmptyList(nativeFields.head, nativeFields.tail)))
    }
  }

  def fromFieldType(field: FieldDescriptorProto, files: List[FileDescriptorProto]): NativeDescriptor =
    field.getType match {
      case Type.TYPE_BOOL     => NativeBool()
      case Type.TYPE_BYTES    => NativeBytes()
      case Type.TYPE_DOUBLE   => NativeDouble()
      case Type.TYPE_FIXED32  => NativeFixed32()
      case Type.TYPE_FIXED64  => NativeFixed64()
      case Type.TYPE_FLOAT    => NativeFloat()
      case Type.TYPE_INT32    => NativeInt32()
      case Type.TYPE_INT64    => NativeInt64()
      case Type.TYPE_SFIXED32 => NativeFixed32()
      case Type.TYPE_SFIXED64 => NativeFixed64()
      case Type.TYPE_SINT32   => NativeInt32()
      case Type.TYPE_SINT64   => NativeInt64()
      case Type.TYPE_STRING   => NativeString()
      case Type.TYPE_UINT32   => NativeInt32()
      case Type.TYPE_UINT64   => NativeInt64()
      case Type.TYPE_ENUM =>
        findEnum(field.getTypeName, files)
          .fold[NativeDescriptor](NativeNull())(e => NativeNamedType(e.getName))
      case Type.TYPE_MESSAGE =>
        findMessage(field.getTypeName, files)
          .fold[NativeDescriptor](NativeNull())(e => NativeNamedType(e.getName))
      case _ => NativeNull()
    }

  def fromFieldOptionsMsg(options: FieldOptions): List[NativeOption] =
    NativeOption("deprecated", options.getDeprecated.toString) ::
      options.getUninterpretedOptionList.j2s.map(t => NativeOption(toString(t.getNameList.j2s), t.getIdentifierValue))

  def fromFieldOptionsEnum(options: EnumOptions): List[NativeOption] = {
    List(
      NativeOption("allow_alias", options.getAllowAlias.toString),
      NativeOption("deprecated", options.getDeprecated.toString)) ++
      options.getUninterpretedOptionList.j2s.map(t => NativeOption(toString(t.getNameList.j2s), t.getIdentifierValue))
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
