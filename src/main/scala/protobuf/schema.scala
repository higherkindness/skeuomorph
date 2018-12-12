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
package protobuf

import cats.Functor
import com.google.protobuf.descriptor.{EnumOptions, FieldDescriptorProto, FileOptions, UninterpretedOption}
import qq.droste.Coalgebra
import scalapb.descriptors._

sealed trait ProtobufF[A]
object ProtobufF {
  final case class Field[A](name: String, tpe: A, position: Int, options: List[Option])
  final case class Option(name: String, value: String)

  final case class TDouble[A]()                extends ProtobufF[A]
  final case class TFloat[A]()                 extends ProtobufF[A]
  final case class TInt32[A]()                 extends ProtobufF[A]
  final case class TInt64[A]()                 extends ProtobufF[A]
  final case class TUint32[A]()                extends ProtobufF[A]
  final case class TUint64[A]()                extends ProtobufF[A]
  final case class TSint32[A]()                extends ProtobufF[A]
  final case class TSint64[A]()                extends ProtobufF[A]
  final case class TFixed32[A]()               extends ProtobufF[A]
  final case class TFixed64[A]()               extends ProtobufF[A]
  final case class TSfixed32[A]()              extends ProtobufF[A]
  final case class TSfixed64[A]()              extends ProtobufF[A]
  final case class TBool[A]()                  extends ProtobufF[A]
  final case class TString[A]()                extends ProtobufF[A]
  final case class TBytes[A]()                 extends ProtobufF[A]
  final case class TNamedType[A](name: String) extends ProtobufF[A]
  final case class TRequired[A](value: A)      extends ProtobufF[A]
  final case class TOptional[A](value: A)      extends ProtobufF[A]
  final case class TRepeated[A](value: A)      extends ProtobufF[A]
  final case class TEnum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[Option],
      aliases: List[(String, Int)])
      extends ProtobufF[A]
  final case class TMessage[A](name: String, fields: List[Field[A]], reserved: List[List[String]]) extends ProtobufF[A]
  final case class TFileDescriptor[A](values: List[A], options: List[Option])                      extends ProtobufF[A]

  implicit val protobufFunctor: Functor[ProtobufF] = new Functor[ProtobufF] {
    def map[A, B](fa: ProtobufF[A])(f: A => B): ProtobufF[B] = fa match {
      case TDouble()        => TDouble()
      case TFloat()         => TFloat()
      case TInt32()         => TInt32()
      case TInt64()         => TInt64()
      case TUint32()        => TUint32()
      case TUint64()        => TUint64()
      case TSint32()        => TSint32()
      case TSint64()        => TSint64()
      case TFixed32()       => TFixed32()
      case TFixed64()       => TFixed64()
      case TSfixed32()      => TSfixed32()
      case TSfixed64()      => TSfixed64()
      case TBool()          => TBool()
      case TString()        => TString()
      case TBytes()         => TBytes()
      case TNamedType(name) => TNamedType(name)
      case TRequired(value) => TRequired(f(value))
      case TOptional(value)                       => TOptional(f(value))
      case TRepeated(value)                       => TRepeated(f(value))
      case TEnum(name, symbols, options, aliases) => TEnum(name, symbols, options, aliases)
      case TMessage(name, fields, reserved) =>
        TMessage(
          name,
          fields.map(field => field.copy(tpe = f(field.tpe))),
          reserved
        )
      case TFileDescriptor(messages, options) => TFileDescriptor(messages.map(f), options)
    }
  }

  def fromProtobuf: Coalgebra[ProtobufF, BaseDescriptor] = Coalgebra { base: BaseDescriptor =>
    base match {
      case f: FileDescriptor => fileFromScala(f)
      case e: EnumDescriptor => enumFromScala(e)
      case d: Descriptor     => messageFromScala(d)
//      case f: FieldDescriptor if f.isRequired                                           => TRequired(f)
//      case f: FieldDescriptor if f.isOptional                                           => TOptional(f)
//      case f: FieldDescriptor if f.isRepeated                                           => TRepeated(f)
      case f: FieldDescriptor if f.name.nonEmpty                                        => TNamedType(f.name) // TODO double check ???
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
    val options = fileDescriptor.getOptions
    val defaultOptions = List(
      ("java_package", options.getJavaPackage),
      ("cc_enable_arenas", options.getCcEnableArenas),
      ("cc_generic_services", options.getCcGenericServices),
      ("deprecated", options.getDeprecated),
      ("java_generate_equals_and_hash", options.getJavaGenerateEqualsAndHash),
      ("java_outer_classname", options.getJavaOuterClassname),
      ("java_multiple_files", options.getJavaMultipleFiles),
      ("java_generic_services", options.getJavaGenericServices),
      ("java_string_check_utf8", options.getJavaStringCheckUtf8),
      ("cc_enable_arenas", options.getCcEnableArenas)
    )

    TFileDescriptor(
      fileDescriptor.messages.toList ++ fileDescriptor.enums.toList,
      Options.options(
        fileDescriptor.getOptions,
        defaultOptions,
        (fileOptions: FileOptions) => fileOptions.uninterpretedOption)
    )
  }

  def enumFromScala(e: EnumDescriptor): TEnum[BaseDescriptor] = {
    val values = e.values.map(value => (value.name, value.number)).toList

    val defaultOptions = List(("allow_alias", e.getOptions.getAllowAlias), ("deprecated", e.getOptions.getDeprecated))

    TEnum(
      e.name,
      values,
      Options
        .options(e.getOptions, defaultOptions, (enumDescriptor: EnumOptions) => enumDescriptor.uninterpretedOption),
      values.groupBy(_._2).values.filter(_.lengthCompare(1) > 0).flatten.toList
    )
  }

  def messageFromScala(descriptor: Descriptor): TMessage[BaseDescriptor] = {
    val options = descriptor.getOptions
    val defaultOptions = List(
      ("deprecated", options.getDeprecated),
      ("map_entry", options.getMapEntry),
      ("message_set_wire_format", options.getMessageSetWireFormat),
      ("no_standard_descriptor_accessor", options.getNoStandardDescriptorAccessor)
    )

    val fields: List[Field[BaseDescriptor]] =
      descriptor.fields
        .map(
          fieldDesc =>
            Field[BaseDescriptor](
              fieldDesc.name,
              fieldDesc,
              fieldDesc.number,
              Options.options(descriptor, defaultOptions, (d: Descriptor) => d.getOptions.uninterpretedOption)))
        .toList
    val reserved: List[List[String]] =
      descriptor.asProto.reservedRange.map(range => (range.getStart to range.getEnd).map(_.toString).toList).toList
    TMessage(descriptor.name, fields, reserved)
  }
}
