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
    case ProtobufF.TDouble()   => fieldTest(desc, TYPE_DOUBLE)
    case ProtobufF.TFloat()    => fieldTest(desc, TYPE_FLOAT)
    case ProtobufF.TInt32()    => fieldTest(desc, TYPE_INT32)
    case ProtobufF.TInt64()    => fieldTest(desc, TYPE_INT64)
    case ProtobufF.TUint32()   => fieldTest(desc, TYPE_UINT32)
    case ProtobufF.TUint64()   => fieldTest(desc, TYPE_UINT64)
    case ProtobufF.TSint32()   => fieldTest(desc, TYPE_SINT32)
    case ProtobufF.TSint64()   => fieldTest(desc, TYPE_SINT64)
    case ProtobufF.TFixed32()  => fieldTest(desc, TYPE_FIXED32)
    case ProtobufF.TFixed64()  => fieldTest(desc, TYPE_FIXED64)
    case ProtobufF.TSfixed32() => fieldTest(desc, TYPE_SFIXED32)
    case ProtobufF.TSfixed64() => fieldTest(desc, TYPE_SFIXED64)
    case ProtobufF.TBool()     => fieldTest(desc, TYPE_BOOL)
    case ProtobufF.TString()   => fieldTest(desc, TYPE_STRING)
    case ProtobufF.TBytes()    => fieldTest(desc, TYPE_BYTES)
    case ProtobufF.TNamedType(n) =>
      desc match {
        case f: FieldDescriptor => f.name == n
        case _                  => false
      }
    case ProtobufF.TRequired(_) =>
      desc match {
        case f: FieldDescriptor => f.isRequired
        case _                  => false
      }
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

  def fieldTest(fieldDesc: BaseDescriptor, targetType: Type): Boolean =
    fieldDesc match {
      case f: FieldDescriptor => f.asProto.getType == targetType
      case _                  => false
    }

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
