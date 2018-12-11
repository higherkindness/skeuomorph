package protobuf

package protobuf

import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import com.google.protobuf.descriptor.FieldDescriptorProto.Type._
import org.scalacheck.Prop
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import scalapb.descriptors._
import qq.droste._
import skeuomorph.protobuf.ProtobufF


// An original version of the Specfile exhibiting the type mismatch error that I'm trying to get to compile
class ProtobufSpec1 extends Specification with ScalaCheck {

  import skeuomorph.avro.protobuf.instances._

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
    case ProtobufF.TDouble()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_DOUBLE))
    case ProtobufF.TFloat()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_FLOAT))
    case ProtobufF.TInt32()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_INT32))
    case ProtobufF.TInt64()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_INT64))
    case ProtobufF.TUint32()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_UINT32))
    case ProtobufF.TUint64()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_UINT64))
    case ProtobufF.TSint32()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SINT32))
    case ProtobufF.TSint64()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SINT64))
    case ProtobufF.TFixed32()     => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_FIXED32))
    case ProtobufF.TFixed64()     => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_FIXED64))
    case ProtobufF.TSfixed32()    => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SFIXED32))
    case ProtobufF.TSfixed64()    => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_SFIXED64))
    case ProtobufF.TBool()        => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_BOOL))
    case ProtobufF.TString()      => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_STRING))
    case ProtobufF.TBytes()       => walkDescriptor(desc, (f: FieldDescriptor) => fieldTest(f, TYPE_BYTES))
    case ProtobufF.TNamedType(n)  => walkDescriptor(desc, (f: FieldDescriptor) => f.name == n)
    case ProtobufF.TRequired(_)   => walkDescriptor(desc, (f: FieldDescriptor) => f.isRequired)
    case ProtobufF.TOptional(_)   => walkDescriptor(desc, (f: FieldDescriptor) => f.isOptional)
    case ProtobufF.TRepeated(_)   => walkDescriptor(desc, (f: FieldDescriptor) => f.isRepeated)
    case e: ProtobufF.TEnum[Boolean] => walkDescriptor[EnumDescriptor](desc, (eDesc: EnumDescriptor) => enumTest(e, eDesc))
    case m: ProtobufF.TMessage[Boolean] => walkDescriptor[Descriptor](desc, (mDesc: Descriptor) => messageTest(m, mDesc))
    case _: ProtobufF.TFileDescriptor[Boolean] => true
  }

  def walkDescriptor[A <: BaseDescriptor](fileDesc: BaseDescriptor, test: A => Boolean): Boolean = {
    fileDesc match {
      case _: FileDescriptor => true // TODO
      case e: EnumDescriptor => test(e)
      case d: Descriptor => test(d)
      case f: FieldDescriptor => test(f)
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

  def messageTest[B](m: ProtobufF.TMessage[B], a: Descriptor): Boolean = {
    // TODO: remainder of message
    a.fullName == m.name
  }

}

