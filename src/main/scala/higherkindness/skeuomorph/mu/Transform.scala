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

package higherkindness.skeuomorph.mu

import higherkindness.skeuomorph.avro.AvroF
import higherkindness.skeuomorph.protobuf.ProtobufF
import qq.droste.Trans

object Transform {

  import MuF._

  /**
   * transform Protobuf schema into Mu schema
   */
  def transformProto[A]: Trans[ProtobufF, MuF, A] = Trans {
    case ProtobufF.TNull()                       => TAtom(ANull)
    case ProtobufF.TDouble()                     => TAtom(ADouble)
    case ProtobufF.TFloat()                      => TAtom(AFloat)
    case ProtobufF.TInt32()                      => TAtom(AInt)
    case ProtobufF.TInt64()                      => TAtom(ALong)
    case ProtobufF.TUint32()                     => TAtom(AInt)
    case ProtobufF.TUint64()                     => TAtom(ALong)
    case ProtobufF.TSint32()                     => TAtom(AInt)
    case ProtobufF.TSint64()                     => TAtom(ALong)
    case ProtobufF.TFixed32()                    => TAtom(AInt)
    case ProtobufF.TFixed64()                    => TAtom(ALong)
    case ProtobufF.TSfixed32()                   => TAtom(AInt)
    case ProtobufF.TSfixed64()                   => TAtom(ALong)
    case ProtobufF.TBool()                       => TAtom(ABoolean)
    case ProtobufF.TString()                     => TAtom(AString)
    case ProtobufF.TBytes()                      => TAtom(AByteArray)
    case ProtobufF.TNamedType(name)              => TNamedType(name)
    case ProtobufF.TRepeated(value)              => TList(value)
    case ProtobufF.TEnum(name, symbols, _, _)    => TSum(name, symbols.map(_._1))
    case ProtobufF.TMessage(name, fields, _)     => TProduct(name, fields.map(f => Field(f.name, f.tpe)))
    case ProtobufF.TFileDescriptor(values, _, _) => TContaining(values)
    case ProtobufF.TOneOf(_, fields)             => TCoproduct(fields.map(_.tpe))
    case ProtobufF.TMap(key, values)             => TMap(Some(key), values)
  }

  def transformAvro[A]: Trans[AvroF, MuF, A] = Trans {
    case AvroF.TNull()          => TAtom(ANull)
    case AvroF.TBoolean()       => TAtom(ABoolean)
    case AvroF.TInt()           => TAtom(AInt)
    case AvroF.TLong()          => TAtom(ALong)
    case AvroF.TFloat()         => TAtom(AFloat)
    case AvroF.TDouble()        => TAtom(ADouble)
    case AvroF.TBytes()         => TAtom(AByteArray)
    case AvroF.TString()        => TAtom(AString)
    case AvroF.TNamedType(name) => TNamedType(name)
    case AvroF.TArray(item)     => TList(item)
    case AvroF.TMap(values)     => TMap(None, values)
    case AvroF.TRecord(name, _, _, _, fields) =>
      TProduct(name, fields.map(f => Field(f.name, f.tpe)))
    case AvroF.TEnum(name, _, _, _, symbols) => TSum(name, symbols)
    case AvroF.TUnion(options)               => TCoproduct(options)
    case AvroF.TFixed(_, _, _, _) =>
      ??? // I don't really know what to do with Fixed... https://avro.apache.org/docs/current/spec.html#Fixed
  }

}
