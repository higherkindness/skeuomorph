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

import cats.Functor
import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import com.google.protobuf.descriptor.FieldDescriptorProto.Type._
import org.scalacheck.Prop
import org.scalatest.{FlatSpec, Matchers}
import scalapb.descriptors._
import qq.droste._
import org.scalatest.prop.Checkers._

class ProtobufSpec extends FlatSpec with Matchers {

  import higherkindness.skeuomorph.instances._

  "ProtobufF.fromProtobuf" should "translate proto files" in {
    check(convertBaseDescriptor)
  }

  def convertBaseDescriptor = Prop.forAll { fileDescriptor: FileDescriptor =>
    val test: BaseDescriptor => Boolean = scheme.hylo(checkProto(fileDescriptor), ProtobufF.fromProtobuf)

    test(fileDescriptor)
  }

  def checkProto(desc: BaseDescriptor): Algebra[ProtobufF, Boolean] = Algebra {
    case ProtobufF.TDouble()                   => fieldTest(desc, Left(TYPE_DOUBLE))
    case ProtobufF.TFloat()                    => fieldTest(desc, Left(TYPE_FLOAT))
    case ProtobufF.TInt32()                    => fieldTest(desc, Left(TYPE_INT32))
    case ProtobufF.TInt64()                    => fieldTest(desc, Left(TYPE_INT64))
    case ProtobufF.TUint32()                   => fieldTest(desc, Left(TYPE_UINT32))
    case ProtobufF.TUint64()                   => fieldTest(desc, Left(TYPE_UINT64))
    case ProtobufF.TSint32()                   => fieldTest(desc, Left(TYPE_SINT32))
    case ProtobufF.TSint64()                   => fieldTest(desc, Left(TYPE_SINT64))
    case ProtobufF.TFixed32()                  => fieldTest(desc, Left(TYPE_FIXED32))
    case ProtobufF.TFixed64()                  => fieldTest(desc, Left(TYPE_FIXED64))
    case ProtobufF.TSfixed32()                 => fieldTest(desc, Left(TYPE_SFIXED32))
    case ProtobufF.TSfixed64()                 => fieldTest(desc, Left(TYPE_SFIXED64))
    case ProtobufF.TBool()                     => fieldTest(desc, Left(TYPE_BOOL))
    case ProtobufF.TString()                   => fieldTest(desc, Left(TYPE_STRING))
    case ProtobufF.TBytes()                    => fieldTest(desc, Left(TYPE_BYTES))
    case ProtobufF.TOneOf(_, _)                => true
    case _: ProtobufF.TMap[Boolean]            => true
    case ProtobufF.TRepeated(_)                => true
    case e: ProtobufF.TEnum[Boolean]           => fieldTest(desc, Right(e))
    case m @ ProtobufF.TMessage(_, _, _)       => fieldTest(desc, Right(m))
    case f: ProtobufF.TFileDescriptor[Boolean] => f.values.forall(v => v)
    case ProtobufF.TNamedType(_)               => true
  }

  def fieldTest(fieldDesc: BaseDescriptor, target: Either[Type, ProtobufF[Boolean]]): Boolean =
    fieldDesc match {
      case f: FileDescriptor =>
        (f.enums ++ f.messages).forall(
          d =>
            // These subsequent hylomorphisms ensure we actually test leaf nodes
            scheme.hylo(checkProto(d), ProtobufF.fromProtobuf)(implicitly[Functor[ProtobufF]])(d))
      case d: Descriptor =>
        target match {
          case Right(m: ProtobufF.TMessage[Boolean]) =>
            m.name == d.name &&
              d.fields.forall(f =>
                scheme.hylo(checkProto(f), ProtobufF.fromProtobuf)(implicitly[Functor[ProtobufF]])(f))
          case _ => true
        }
      case e: EnumDescriptor =>
        target match {
          case Right(tEnum: ProtobufF.TEnum[Boolean]) =>
            e.name == tEnum.name &&
              e.values.length == tEnum.symbols.length + tEnum.aliases.length &&
              tEnum.symbols.head._2 == 0 // The first Enum must start with 0 according to protobuf
          case _ => false
        }
      case f: FieldDescriptor =>
        val Left(targetType) = target
        targetType == f.asProto.getType
      case _ => true
    }
}
