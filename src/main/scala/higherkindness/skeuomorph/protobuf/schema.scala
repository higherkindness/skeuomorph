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

package higherkindness.skeuomorph.protobuf

import cats.implicits._
import com.google.protobuf.descriptor.{EnumOptions, FieldDescriptorProto, UninterpretedOption}
import qq.droste.Coalgebra
import qq.droste.macros.deriveTraverse
import scalapb.descriptors.{ScalaType, _}

@deriveTraverse
sealed trait ProtobufF[A]

object ProtobufF {
  @deriveTraverse
  final case class Field[A](name: String, tpe: A, position: Int, options: List[Option], isRepeated: Boolean)

  final case class Option(name: String, value: String)

  final case class TDouble[A]()                   extends ProtobufF[A]
  final case class TFloat[A]()                    extends ProtobufF[A]
  final case class TInt32[A]()                    extends ProtobufF[A]
  final case class TInt64[A]()                    extends ProtobufF[A]
  final case class TUint32[A]()                   extends ProtobufF[A]
  final case class TUint64[A]()                   extends ProtobufF[A]
  final case class TSint32[A]()                   extends ProtobufF[A]
  final case class TSint64[A]()                   extends ProtobufF[A]
  final case class TFixed32[A]()                  extends ProtobufF[A]
  final case class TFixed64[A]()                  extends ProtobufF[A]
  final case class TSfixed32[A]()                 extends ProtobufF[A]
  final case class TSfixed64[A]()                 extends ProtobufF[A]
  final case class TBool[A]()                     extends ProtobufF[A]
  final case class TString[A]()                   extends ProtobufF[A]
  final case class TBytes[A]()                    extends ProtobufF[A]
  final case class TNamedType[A](name: String)    extends ProtobufF[A]
  final case class TRepeated[A](value: A)         extends ProtobufF[A]
  final case class TOneOf[A](invariants: List[A]) extends ProtobufF[A]

  final case class TEnum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[Option],
      aliases: List[(String, Int)])
      extends ProtobufF[A]

  final case class TMessage[A](name: String, fields: List[Field[A]], reserved: List[List[String]]) extends ProtobufF[A]

  final case class TFileDescriptor[A](values: List[A], name: String, `package`: String) extends ProtobufF[A]

  def fromProtobuf: Coalgebra[ProtobufF, BaseDescriptor] = Coalgebra { base: BaseDescriptor =>
    base match {
      case f: FileDescriptor                                                            => fileFromScala(f)
      case e: EnumDescriptor                                                            => enumFromScala(e)
      case o: Descriptor if o.oneofs.nonEmpty                                           => TOneOf(o.oneofs.flatMap(oof => oof.fields).toList)
      case d: Descriptor                                                                => messageFromScala(d)
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_BOOL     => TBool()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_BYTES    => TBytes()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_DOUBLE   => TDouble()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_FIXED32  => TFixed32()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_FIXED64  => TFixed64()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_FLOAT    => TFloat()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_INT32    => TInt32()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_INT64    => TInt64()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_SFIXED32 => TFixed32()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_SFIXED64 => TFixed64()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_SINT32   => TSint32()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_SINT64   => TSint64()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_STRING   => TString()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_UINT32   => TUint32()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_UINT64   => TUint64()
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_MESSAGE  => getNestedTypeName(f)
      case f: FieldDescriptor if f.protoType == FieldDescriptorProto.Type.TYPE_ENUM     => getNestedTypeName(f)
    }
  }

  object Options {
    import com.google.protobuf.descriptor.UninterpretedOption.NamePart

    def options[A, B](a: A, defaultFlags: List[(String, B)], f: A => Seq[UninterpretedOption]): List[Option] =
      (defaultFlags.map(e => (e._1, s"${e._2}")) ++ uninterpretedOptions(a, f)).map {
        case (name, value) => Option(name, value)
      }

    private def uninterpretedOptions[A](a: A, f: A => Seq[UninterpretedOption]): Seq[(String, String)] =
      f(a).flatMap(o => o.identifierValue.map((toString(o.name), _)))

    private def toString(nameParts: Seq[NamePart]): String =
      nameParts.foldLeft("")((l, r) => if (r.isExtension) s"$l.($r)" else s"$l.$r")
  }

  def fileFromScala(fileDescriptor: FileDescriptor): TFileDescriptor[BaseDescriptor] = {
    TFileDescriptor(
      fileDescriptor.messages.toList ++ fileDescriptor.enums.toList,
      fileDescriptor.fullName,
      fileDescriptor.packageName
    )
  }

  def enumFromScala(e: EnumDescriptor): TEnum[BaseDescriptor] = {
    val defaultOptions = List(("allow_alias", e.getOptions.getAllowAlias), ("deprecated", e.getOptions.getDeprecated))

    val valuesAndAliases = e.values.map(value => (value.name, value.number))
    val (hasAlias, noAlias) = valuesAndAliases
      .groupBy(_._2)
      .values
      .partition(_.lengthCompare(1) > 0)

    val separateValueFromAliases = hasAlias.map(list => (list.head, list.tail))
    val values = separateValueFromAliases.map(_._1) ++ noAlias.flatten
    val aliases = separateValueFromAliases.flatMap(_._2)

    TEnum(
      e.name,
      values.toList,
      Options
        .options(e.getOptions, defaultOptions, (enumDescriptor: EnumOptions) => enumDescriptor.uninterpretedOption),
      aliases.toList
    )
  }

  def messageFromScala(descriptor: Descriptor): TMessage[BaseDescriptor] = {
    val options = descriptor.getOptions
    val defaultOptions = List(
      ("deprecated", options.getDeprecated),
      ("map_entry", options.getMapEntry)
    )

    val fields: List[Field[BaseDescriptor]] =
      descriptor.fields
        .map(
          fieldDesc =>
            Field[BaseDescriptor](
              fieldDesc.name,
              fieldDesc,
              fieldDesc.number,
              Options.options(descriptor, defaultOptions, (d: Descriptor) => d.getOptions.uninterpretedOption),
              fieldDesc.isRepeated
          )
        )
        .toList
    val reserved: List[List[String]] =
      descriptor.asProto.reservedRange.map(range => (range.getStart until range.getEnd).map(_.toString).toList).toList
    TMessage(descriptor.name, fields, reserved)
  }

  def getNestedTypeName(f: FieldDescriptor): TNamedType[BaseDescriptor] = {
    f.scalaType match {
      case ScalaType.Message(descriptor) => TNamedType(descriptor.name)
      case ScalaType.Enum(enumDesc)      => TNamedType(enumDesc.name)
      case _: ScalaType                  => TNamedType("Unknown") // TODO: what should be done here?
    }
  }
}
