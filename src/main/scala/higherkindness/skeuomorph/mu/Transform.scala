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
import higherkindness.droste.{Embed, Trans}

object Transform {

  import MuF._

  /**
   * transform Protobuf schema into Mu schema
   */
  def transformProto[A](implicit A: Embed[MuF, A]): Trans[ProtobufF, MuF, A] = Trans {
    case ProtobufF.TNull()                          => TNull()
    case ProtobufF.TDouble()                        => TDouble()
    case ProtobufF.TFloat()                         => TFloat()
    case ProtobufF.TInt32()                         => TInt()
    case ProtobufF.TInt64()                         => TLong()
    case ProtobufF.TUint32()                        => TInt()
    case ProtobufF.TUint64()                        => TLong()
    case ProtobufF.TSint32()                        => TInt()
    case ProtobufF.TSint64()                        => TLong()
    case ProtobufF.TFixed32()                       => TInt()
    case ProtobufF.TFixed64()                       => TLong()
    case ProtobufF.TSfixed32()                      => TInt()
    case ProtobufF.TSfixed64()                      => TLong()
    case ProtobufF.TBool()                          => TBoolean()
    case ProtobufF.TString()                        => TString()
    case ProtobufF.TBytes()                         => TByteArray()
    case ProtobufF.TNamedType(prefix, name)         => TNamedType(prefix, name)
    case ProtobufF.TOptionalNamedType(prefix, name) => TOption(A.algebra(TNamedType(prefix, name)))
    case ProtobufF.TRepeated(value)                 => TList(value)
    case ProtobufF.TEnum(name, symbols, _, _)       => TSum(name, symbols.map(SumField.tupled))
    case ProtobufF.TMessage(name, fields, _)        => TProduct(name, fields.map(f => Field(f.name, f.tpe, f.indices)))
    case ProtobufF.TFileDescriptor(values, _, _)    => TContaining(values)
    case ProtobufF.TOneOf(_, fields)                => TCoproduct(fields.map(_.tpe))
    case ProtobufF.TMap(key, values)                => TMap(Some(key), values)
  }

  def transformAvro[A]: Trans[AvroF, MuF, A] = Trans {
    case AvroF.TNull()          => TNull()
    case AvroF.TBoolean()       => TBoolean()
    case AvroF.TInt()           => TInt()
    case AvroF.TLong()          => TLong()
    case AvroF.TFloat()         => TFloat()
    case AvroF.TDouble()        => TDouble()
    case AvroF.TBytes()         => TByteArray()
    case AvroF.TString()        => TString()
    case AvroF.TNamedType(name) => TNamedType(Nil, name)
    case AvroF.TArray(item)     => TList(item)
    case AvroF.TMap(values)     => TMap(None, values)
    case AvroF.TRecord(name, _, _, _, fields) =>
      TProduct(name, fields.zipWithIndex.map { case (f, i) => Field(f.name, f.tpe, List(i)) })
    case AvroF.TEnum(name, _, _, _, symbols) => TSum(name, symbols.zipWithIndex.map(SumField.tupled))
    case AvroF.TUnion(options)               => TCoproduct(options)
    case AvroF.TFixed(_, _, _, _) =>
      ??? // I don't really know what to do with Fixed... https://avro.apache.org/docs/current/spec.html#Fixed
  }

}
