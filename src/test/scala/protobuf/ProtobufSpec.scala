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

package protobuf

import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import com.google.protobuf.descriptor.FieldDescriptorProto.Type._
import org.scalacheck.Prop
import org.scalatest.{FlatSpec, Matchers}
import scalapb.descriptors._
import qq.droste._
import skeuomorph.protobuf.ProtobufF
import org.scalatest.prop.Checkers._

class ProtobufSpec extends FlatSpec with Matchers {

  import skeuomorph.avro.protobuf.instances._

  "ProtobufF.fromProtobuf" should "translate proto files" in {
    check(convertBaseDescriptor)
  }

  def convertBaseDescriptor = Prop.forAll { fileDescriptor: FileDescriptor =>
    val test = scheme.hylo(checkProto(fileDescriptor), ProtobufF.fromProtobuf)

    test(fileDescriptor)
  }

  def checkProto(desc: BaseDescriptor): Algebra[ProtobufF, Boolean] = Algebra {
    case ProtobufF.TDouble() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_DOUBLE)
        case _                  => false
      }
    case ProtobufF.TFloat() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_FLOAT)
        case _                  => false
      }
    case ProtobufF.TInt32() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_INT32)
        case _                  => false
      }
    case ProtobufF.TInt64() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_INT64)
        case _                  => false
      }
    case ProtobufF.TUint32() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_UINT32)
        case _                  => false
      }
    case ProtobufF.TUint64() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_UINT64)
        case _                  => false
      }
    case ProtobufF.TSint32() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_SINT32)
        case _                  => false
      }
    case ProtobufF.TSint64() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_SINT64)
        case _                  => false
      }
    case ProtobufF.TFixed32() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_FIXED32)
        case _                  => false
      }
    case ProtobufF.TFixed64() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_FIXED64)
        case _                  => false
      }
    case ProtobufF.TSfixed32() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_SFIXED32)
        case _                  => false
      }
    case ProtobufF.TSfixed64() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_SFIXED64)
        case _                  => false
      }
    case ProtobufF.TBool() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_BOOL)
        case _                  => false
      }
    case ProtobufF.TString() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_STRING)
        case _                  => false
      }
    case ProtobufF.TBytes() =>
      desc match {
        case f: FieldDescriptor => fieldTest(f, TYPE_BYTES)
        case _                  => false
      }
    case ProtobufF.TNamedType(n) =>
      desc match {
        case f: FieldDescriptor => f.name == n
        case _                  => false
      }
//    case ProtobufF.TRequired(r) =>
//      desc match {
//        case f: FieldDescriptor if f.isRequired => r
//        case _                  => false
//      }
//    case ProtobufF.TOptional(o) =>
//      desc match {
//        case f: FieldDescriptor if f.isOptional => o
//        case _                  => false
//      }
//    case ProtobufF.TRepeated(r) =>
//      desc match {
//        case f: FieldDescriptor if f.isRepeated => r
//        case _                  => false
//      }
    case e: ProtobufF.TEnum[Boolean] =>
      desc match {
        case eDesc: EnumDescriptor => enumTest(e, eDesc)
        case _                     => false
      }
    case m: ProtobufF.TMessage[Boolean] =>
      desc match {
        case mDesc: Descriptor => messageTest(m, mDesc)
        case _                 => false
      }
    case f: ProtobufF.TFileDescriptor[Boolean] =>
      desc match {
        case fDesc: FileDescriptor => fileDescriptorTest(f, fDesc)
        case _                     => false
      }
  }

  def fieldTest(fieldDesc: FieldDescriptor, targetType: Type): Boolean =
    fieldDesc.asProto.getType == targetType

  def enumTest[B](protoEnum: ProtobufF.TEnum[B], enumDesc: EnumDescriptor): Boolean = {
    // Assuming we don't care about order
    val protoSymbols = protoEnum.symbols.toSet
    val nameBool     = protoEnum.name == enumDesc.name
    val symbolsBool  = protoSymbols.diff(enumDesc.values.map(evd => (evd.name, evd.number)).toSet) == Set.empty
    val optionsBool  = true // TODO pending options implementation from Oli
    val aliasBool    = true // TODO: pending alias implementation

    nameBool && symbolsBool && optionsBool && aliasBool
  }

  def messageTest[B](m: ProtobufF.TMessage[B], messageDesc: Descriptor): Boolean =
    messageDesc.fullName == m.name &&
    m.fields.length == messageDesc.fields.length

  def fileDescriptorTest[B](f: ProtobufF.TFileDescriptor[B], fileDesc: FileDescriptor): Boolean =
    fileDesc.enums.length + fileDesc.messages.length == f.values.length
}
