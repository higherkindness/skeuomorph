package protobuf

import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import com.google.protobuf.descriptor.FieldDescriptorProto.Type._
import org.scalacheck.Prop
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import scalapb.descriptors._
import qq.droste._
import skeuomorph.protobuf.ProtobufF

class ProtobufSpec extends Specification with ScalaCheck {

  import skeuomorph.avro.protobuf.instances._

  override def is: SpecStructure = s2"""
  Protobuf Schema

  It should be possible to create a ProtoF TMessage from a BaseDescriptor. $convertBaseDescriptor

  """

  def convertBaseDescriptor = Prop.forAll { fileDescriptor: FileDescriptor =>
    val test = scheme.hylo(checkProto(fileDescriptor), ProtobufF.fromProtobuf)

    test(fileDescriptor)
  }

  def checkProto(desc: BaseDescriptor): Algebra[ProtobufF, Boolean] = Algebra {
    case ProtobufF.TDouble() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_DOUBLE)
      case _ => false
    }
    case ProtobufF.TFloat() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_FLOAT)
      case _ => false
    }
    case ProtobufF.TInt32() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_INT32)
      case _ => false
    }
    case ProtobufF.TInt64() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_INT64)
      case _ => false
    }
    case ProtobufF.TUint32() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_UINT32)
      case _ => false
    }
    case ProtobufF.TUint64() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_UINT64)
      case _ => false
    }
    case ProtobufF.TSint32() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_SINT32)
      case _ => false
    }
    case ProtobufF.TSint64() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_SINT64)
      case _ => false
    }
    case ProtobufF.TFixed32() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_FIXED32)
      case _ => false
    }
    case ProtobufF.TFixed64() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_FIXED64)
      case _ => false
    }
    case ProtobufF.TSfixed32() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_SFIXED32)
      case _ => false
    }
    case ProtobufF.TSfixed64() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_SFIXED64)
      case _ => false
    }
    case ProtobufF.TBool() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_BOOL)
      case _ => false
    }
    case ProtobufF.TString() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_STRING)
      case _ => false
    }
    case ProtobufF.TBytes() => desc match {
      case f: FieldDescriptor => fieldTest(f, TYPE_BYTES)
      case _ => false
    }
    case ProtobufF.TNamedType(n) => desc match {
      case f: FieldDescriptor => f.name == n
      case _ => false
    }
    case ProtobufF.TRequired(_) => desc match {
      case f: FieldDescriptor => f.isRequired
      case _ => false
    }
    case ProtobufF.TOptional(_) => desc match {
      case f: FieldDescriptor => f.isOptional
      case _ => false
    }
    case ProtobufF.TRepeated(_) => desc match {
      case f: FieldDescriptor => f.isRepeated
      case _ => false
    }
    case e: ProtobufF.TEnum[Boolean] => desc match {
      case eDesc: EnumDescriptor => enumTest(e, eDesc)
      case _ => false
    }
    case m: ProtobufF.TMessage[Boolean] => desc match {
      case mDesc: Descriptor => messageTest(m, mDesc)
      case _ => false
    }
    case f: ProtobufF.TFileDescriptor[Boolean] => desc match {
      case fDesc: FileDescriptor => fileDescriptorTest(f, fDesc)
      case _ => false
    }
  }

  def fieldTest(fieldDesc: FieldDescriptor, targetType: Type): Boolean = {
    fieldDesc.asProto.getType == targetType
  }

  def enumTest[B](protoEnum: ProtobufF.TEnum[B], enumDesc: EnumDescriptor): Boolean = {
    // Assuming we don't care about order
    val protoSymbols = protoEnum.symbols.toSet
    val nameBool = protoEnum.name == enumDesc.name
    val symbolsBool = protoSymbols.diff(enumDesc.values.map(evd => (evd.name, evd.number)).toSet) == Set.empty
    val optionsBool = true // TODO pending options implementation from Oli
    val aliasBool = true // TODO: pending alias implementation

    nameBool && symbolsBool && optionsBool && aliasBool
  }

  def messageTest[B](m: ProtobufF.TMessage[B], messageDesc: Descriptor): Boolean = {
    // TODO: remainder of message
    messageDesc.fullName == m.name
  }

  def fileDescriptorTest[B](f: ProtobufF.TFileDescriptor[B], fileDesc: FileDescriptor): Boolean = {
    // TODO: remainder of file descriptor
    fileDesc.enums.length + fileDesc.messages.length == f.values.length
  }

}

