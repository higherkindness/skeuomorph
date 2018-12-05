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
import com.google.protobuf.descriptor.{DescriptorProto, EnumDescriptorProto, FieldDescriptorProto, FileDescriptorProto}
import qq.droste.Coalgebra

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

  implicit val protobufFunctor: Functor[ProtobufF] = new Functor[ProtobufF] {
    def map[A, B](fa: ProtobufF[A])(f: A => B): ProtobufF[B] = fa match {
      case TDouble()                              => TDouble()
      case TFloat()                               => TFloat()
      case TInt32()                               => TInt32()
      case TInt64()                               => TInt64()
      case TUint32()                              => TUint32()
      case TUint64()                              => TUint64()
      case TSint32()                              => TSint32()
      case TSint64()                              => TSint64()
      case TFixed32()                             => TFixed32()
      case TFixed64()                             => TFixed64()
      case TSfixed32()                            => TSfixed32()
      case TSfixed64()                            => TSfixed64()
      case TBool()                                => TBool()
      case TString()                              => TString()
      case TBytes()                               => TBytes()
      case TNamedType(name)                       => TNamedType(name)
      case TRequired(value)                       => TRequired(f(value))
      case TOptional(value)                       => TOptional(f(value))
      case TRepeated(value)                       => TRepeated(f(value))
      case TEnum(name, symbols, options, aliases) => TEnum(name, symbols, options, aliases)
      case TMessage(name, fields, reserved) =>
        TMessage(
          name,
          fields.map(field => field.copy(tpe = f(field.tpe))),
          reserved
        )
    }
  }

  /* A problem, generally, with this plan, appears to be that by the time you are in a fieldDescriptor (to match on the type),
   you have lost some more global information. I'm not confident that the .getType method would even work on a Message or Enum,
   as it appears Protoc separates Messages from Enums and Services by writing those fields in a separate class altogether.
   */
  def fromProtobuf: Coalgebra[ProtobufF, (FieldDescriptorProto, FileDescriptorProto)] = Coalgebra {
    case (fieldDescriptor: FieldDescriptorProto, file: FileDescriptorProto) =>
      fieldDescriptor.getType match {
        case FieldDescriptorProto.Type.TYPE_BOOL    => TBool()
        case FieldDescriptorProto.Type.TYPE_BYTES   => TBytes()
        case FieldDescriptorProto.Type.TYPE_DOUBLE  => TDouble()
        case FieldDescriptorProto.Type.TYPE_ENUM    => createEnum(fieldDescriptor.getName, file)
        case FieldDescriptorProto.Type.TYPE_FIXED32 => TFixed32()
        case FieldDescriptorProto.Type.TYPE_FIXED64 => TFixed64()
        case FieldDescriptorProto.Type.TYPE_FLOAT   => TFloat()
        case FieldDescriptorProto.Type.TYPE_GROUP   => null // Is this supported in Proto 3???
        case FieldDescriptorProto.Type.TYPE_INT32   => TInt32()
        case FieldDescriptorProto.Type.TYPE_INT64   => TInt64()
        case FieldDescriptorProto.Type.TYPE_MESSAGE => createMessage(fieldDescriptor, file)
        case FieldDescriptorProto.Type.TYPE_SFIXED32 => TFixed32()
        case FieldDescriptorProto.Type.TYPE_SFIXED64 => TFixed64()
        case FieldDescriptorProto.Type.TYPE_SINT32   => TSint32()
        case FieldDescriptorProto.Type.TYPE_SINT64   => TSint64()
        case FieldDescriptorProto.Type.TYPE_STRING   => TString()
        case FieldDescriptorProto.Type.TYPE_UINT32   => TUint32()
        case FieldDescriptorProto.Type.TYPE_UINT64   => TUint64()
        case FieldDescriptorProto.Type.Unrecognized(_) => ??? // TODO: Unrecognized
      }
  }

  def createEnum(enumName: String, fileDescriptorProto: FileDescriptorProto): TEnum[(FieldDescriptorProto, FileDescriptorProto)] = {
    val enumDescriptors: Seq[EnumDescriptorProto] = fileDescriptorProto.enumType.filter(enumDesc => enumDesc.name.contains(enumName))
    val symbols: List[(String, Int)] = enumDescriptors.flatMap(enumDesc => enumDesc.value.map(protoValue => (protoValue.getName, protoValue.getNumber))).toList
    // TODO: Options and Aliases
    TEnum(enumName, symbols, List(), List())
  }

  def createMessage(field: FieldDescriptorProto, file: FileDescriptorProto): TMessage[(FieldDescriptorProto, FileDescriptorProto)] = {
    val filtered = file.messageType.filter(descriptorProto => descriptorProto.name.contains(field.getName))
    // TODO: Options
    val protoFields = filtered.map(desc => Field(desc.getName, (field, file), field.getNumber, List())).toList
    // TODO: Reserved
    TMessage(field.getName, protoFields, List())
  }
}
