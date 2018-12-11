package protobuf

import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import org.scalacheck.Prop
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import scalapb.descriptors._
import qq.droste._
import skeuomorph.protobuf.ProtobufF
import skeuomorph.protobuf.ProtobufF.TEnum

class ProtobufSpec2 extends Specification with ScalaCheck {

  override def is: SpecStructure =
    s2"""
  Protobuf Schema

  It should be possible to create a ProtoF TMessage from a BaseDescriptor. $convertBaseDescriptor

  """

  def convertBaseDescriptor = Prop.forAll { fileDescriptor: FileDescriptor =>
    val test = scheme.hylo(checkProto(fileDescriptor), ProtobufF.fromProtobuf)

    test(fileDescriptor)
  }

  def checkProto(desc: BaseDescriptor): Algebra[ProtobufF, Boolean] = Algebra {
    //    case ProtobufF.TDouble()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_DOUBLE))
    //    case ProtobufF.TFloat()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_FLOAT))
    //    case ProtobufF.TInt32()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_INT32))
    //    case ProtobufF.TInt64()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_INT64))
    //    case ProtobufF.TUint32()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_UINT32))
    //    case ProtobufF.TUint64()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_UINT64))
    //    case ProtobufF.TSint32()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SINT32))
    //    case ProtobufF.TSint64()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SINT64))
    //    case ProtobufF.TFixed32()     => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_FIXED32))
    //    case ProtobufF.TFixed64()     => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_FIXED64))
    //    case ProtobufF.TSfixed32()    => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SFIXED32))
    //    case ProtobufF.TSfixed64()    => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SFIXED64))
    //    case ProtobufF.TBool()        => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_BOOL))
    //    case ProtobufF.TString()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_STRING))
    //    case ProtobufF.TBytes()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_BYTES))
    //    case ProtobufF.TNamedType(n)  => walkDescriptor(desc, (f: FieldDescriptor) => f.name == n)
    //    case ProtobufF.TRequired(_)   => walkDescriptor(desc, (f: FieldDescriptor) => f.isRequired)
    //    case ProtobufF.TOptional(_)   => walkDescriptor(desc, (f: FieldDescriptor) => f.isOptional)
    //    case ProtobufF.TRepeated(_)   => walkDescriptor(desc, (f: FieldDescriptor) => f.isRepeated)
    case e: ProtobufF.TEnum[Boolean] => walkDescriptor[TEnum[Boolean]](desc, e) //new BaseDescriptorTester[EnumDescriptor, ProtobufF.TEnum[Boolean]].enumTest(e, desc)
    //    case m: ProtobufF.TMessage[Boolean] => walkDescriptor[Boolean, Descriptor](desc, (mDesc: Descriptor) => messageTest(m, mDesc))
    //    case _: ProtobufF.TFileDescriptor[Boolean] => true
  }

  def walkDescriptor[B <: ProtobufF[Boolean]](fileDesc: BaseDescriptor, protobufF: B): Boolean = {
    fileDesc match {
      case e: EnumDescriptor => new BaseDescriptorTester[EnumDescriptor, ProtobufF.TEnum[Boolean]].enumTest(e, protobufF) // I'd have to do a double match here and I'd prefer not do to that...
      case d: Descriptor => new BaseDescriptorTester[Descriptor, ProtobufF.TMessage[Boolean]].messageTest(d, protobufF)
      case f: FileDescriptor => new BaseDescriptorTester[FileDescriptor, ProtobufF.TMessage[Boolean]].fileDescriptorTest(f, protobufF)
    }
  }
}

// ATTEMPT 1: Create a class that is parameterized 2 types, one constrained to BaseDescriptors, another constrained to ProtobufF's.
// Does not appear to work.
class BaseDescriptorTester[D <: BaseDescriptor, B <: ProtobufF[Boolean]] {

  def fieldTest(fieldDesc: FieldDescriptor, targetType: Type): Boolean = {
    fieldDesc.asProto.getType == targetType
  }

  def enumTest(enumDesc: D, protoEnum: B)(implicit ev: D =:= EnumDescriptor, ev2: B =:= ProtobufF.TEnum[Boolean]): Boolean = {
    // Assuming we don't care about order
    val protoSymbols = protoEnum.symbols.toSet
    val nameBool = protoEnum.name == enumDesc.name
    val symbolsBool = protoSymbols.diff(enumDesc.values.map(evd => (evd.name, evd.number)).toSet) == Set.empty
    val optionsBool = true // TODO pending options implementation from Oli
    val aliasBool = true // TODO: pending alias implementation

    nameBool && symbolsBool && optionsBool && aliasBool
  }

  def messageTest(messageDesc: D, m: B)(implicit ev: D =:= Descriptor, ev2: B =:= ProtobufF.TMessage[Boolean]): Boolean = {
    // TODO: remainder of message
    messageDesc.fullName == m.name
  }

  def fileDescriptorTest(fileDesc: D, protoM: B)(implicit ev: D =:= FileDescriptor, ev2: B =:= ProtobufF.TFileDescriptor[Boolean]): Boolean = {
    // TODO: remainder of file descriptor
    fileDesc.enums.length == protoM.enums.length && fileDesc.messages.length == protoM.messages.length
  }

}

