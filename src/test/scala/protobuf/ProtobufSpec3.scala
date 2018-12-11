package protobuf

import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import org.scalacheck.Prop
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import scalapb.descriptors._
import qq.droste._
import skeuomorph.protobuf.ProtobufF
import skeuomorph.protobuf.ProtobufF.TEnum

class ProtobufSpec3 extends Specification with ScalaCheck {

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
    case e: ProtobufF.TEnum[Boolean] => walkDescriptor[BaseDescriptor, TEnum[Boolean]](desc, e) //new BaseDescriptorTester[EnumDescriptor, ProtobufF.TEnum[Boolean]].enumTest(e, desc)
    //    case m: ProtobufF.TMessage[Boolean] => walkDescriptor[Boolean, Descriptor](desc, (mDesc: Descriptor) => messageTest(m, mDesc))
    //    case _: ProtobufF.TFileDescriptor[Boolean] => true
  }

    def walkDescriptor[A <: BaseDescriptor, B <: ProtobufF[Boolean]](fileDesc: A, protobufF: B)(implicit tester: ProtoTester[A,B]): Boolean = {
      fileDesc match {
        case e: EnumDescriptor => tester.test(e, protobufF)
        case d: Descriptor => tester.test(d, protobufF)
        case f: FileDescriptor => tester.test(f, protobufF)
      }
    }
}

trait ProtoTester[A, B]{
  def test(a: A, b: B): Boolean
}

object ProtoTesterImpl {

  def apply[A, B](implicit p: ProtoTester[A, B]): ProtoTester[A,B] = p

  implicit val enumTester: ProtoTester[EnumDescriptor, ProtobufF.TEnum[Boolean]] = new ProtoTester[EnumDescriptor, ProtobufF.TEnum[Boolean]] {
    override def test(a: EnumDescriptor, b: TEnum[Boolean]): Boolean = {
    val protoSymbols = b.symbols.toSet
    val nameBool = b.name == a.name
    val symbolsBool = protoSymbols.diff(a.values.map(evd => (evd.name, evd.number)).toSet) == Set.empty
    val optionsBool = true // TODO pending options implementation from Oli
    val aliasBool = true // TODO: pending alias implementation

    nameBool && symbolsBool && optionsBool && aliasBool
    }
  }

  implicit val fileDescriptorTester: ProtoTester[FileDescriptor, ProtobufF.TFileDescriptor[Boolean]] = new ProtoTester[FileDescriptor, ProtobufF.TFileDescriptor[Boolean]] {
    // TODO: better tests for this
    override def test(a: FileDescriptor, b: ProtobufF.TFileDescriptor[Boolean]): Boolean = {
      a.enums.length == b.enums.length &&
      a.messages.length == b.messages.length
    }
  }

  implicit val messageTest: ProtoTester[Descriptor, ProtobufF.TMessage[Boolean]] = new ProtoTester[Descriptor, ProtobufF.TMessage[Boolean]] {
    override def test(a: Descriptor, b: ProtobufF.TMessage[Boolean]): Boolean = ???
  }

}
