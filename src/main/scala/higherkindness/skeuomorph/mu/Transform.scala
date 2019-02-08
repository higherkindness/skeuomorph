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

package higherkindness.skeuomorph
package mu

import cats.implicits._
import higherkindness.skeuomorph.avro
import higherkindness.skeuomorph.protobuf.ProtobufF
import qq.droste._

object Transform {

//  import MuF._

  /**
   * transform Protobuf schema into Mu schema
   */
  def transformProto[A]: Trans[ProtobufF, mu.Type, A] = ???
  // Trans {
  //   case ProtobufF.TDouble()                  => TDouble()
  //   case ProtobufF.TFloat()                   => TFloat()
  //   case ProtobufF.TInt32()                   => TInt()
  //   case ProtobufF.TInt64()                   => TLong()
  //   case ProtobufF.TUint32()                  => TInt()
  //   case ProtobufF.TUint64()                  => TLong()
  //   case ProtobufF.TSint32()                  => TInt()
  //   case ProtobufF.TSint64()                  => TLong()
  //   case ProtobufF.TFixed32()                 => TInt()
  //   case ProtobufF.TFixed64()                 => TLong()
  //   case ProtobufF.TSfixed32()                => TInt()
  //   case ProtobufF.TSfixed64()                => TLong()
  //   case ProtobufF.TBool()                    => TBoolean()
  //   case ProtobufF.TString()                  => TString()
  //   case ProtobufF.TBytes()                   => TByteArray()
  //   case ProtobufF.TNamedType(name)           => TNamedType(name)
  //   case ProtobufF.TOptional(value)           => TOption(value)
  //   case ProtobufF.TRepeated(value)           => TList(value)
  //   case ProtobufF.TRequired(value)           => TRequired(value)
  //   case ProtobufF.TEnum(name, symbols, _, _) => TSum(name, symbols.map(_._1))
  //   case ProtobufF.TMessage(name, fields, _)  => TRecord(name, fields.map(f => Field(f.name, f.tpe)))
  // }

  def transformAvro[A]: TransM[Option, avro.Type, mu.Type, A] = TransM {
    case avro.InjNull(a)      => mu.InjNull(a).some
    case avro.InjBoolean(a)   => mu.InjBoolean(a).some
    case avro.InjInt(a)       => mu.InjInt(a).some
    case avro.InjLong(a)      => mu.InjLong(a).some
    case avro.InjFloat(a)     => mu.InjFloat(a).some
    case avro.InjDouble(a)    => mu.InjDouble(a).some
    case avro.InjByteArray(a) => mu.InjByteArray(a).some
    case avro.InjString(a)    => mu.InjString(a).some
    case avro.InjMap(a)       => mu.InjMap(a).some
    case avro.InjRecord(a)    => mu.InjRecord(a).some
    case avro.InjEnum(a)      => mu.InjEnum(a).some
    case avro.InjUnion(a)     => mu.InjUnion(a).some
    case avro.InjFixed(_)     => none[mu.Type[A]]
  }
  //Trans {
  //   case AvroF.TNull()          => TNull()
  //   case AvroF.TBoolean()       => TBoolean()
  //   case AvroF.TInt()           => TInt()
  //   case AvroF.TLong()          => TLong()
  //   case AvroF.TFloat()         => TFloat()
  //   case AvroF.TDouble()        => TDouble()
  //   case AvroF.TBytes()         => TByteArray()
  //   case AvroF.TString()        => TString()
  //   case AvroF.TNamedType(name) => TNamedType(name)
  //   case AvroF.TArray(item)     => TList(item)
  //   case AvroF.TMap(values)     => TMap(values)
  //   case AvroF.TRecord(name, _, _, _, fields) =>
  //     TRecord(name, fields.map(f => Field(f.name, f.tpe)))
  //   case AvroF.TEnum(name, _, _, _, symbols) => TSum(name, symbols)
  //   case AvroF.TUnion(options)               => TCoproduct(options)
  //   case AvroF.TFixed(_, _, _, _) =>
  //     ??? // I don't really know what to do with Fixed... https://avro.apache.org/docs/current/spec.html#Fixed
  // }


}
