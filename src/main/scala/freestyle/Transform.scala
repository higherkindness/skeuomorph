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

package skeuomorph
package freestyle

import protobuf.{Schema => ProtoSchema}
import avro.{Schema => AvroSchema}
import qq.droste.Trans

object Transform {

  import Schema._

  /**
   * transform Protobuf schema into Freestyle schema
   */
  def transformProto[A]: Trans[ProtoSchema, Schema, A] = Trans {
    case ProtoSchema.TDouble()                  => TDouble()
    case ProtoSchema.TFloat()                   => TFloat()
    case ProtoSchema.TInt32()                   => TInt()
    case ProtoSchema.TInt64()                   => TLong()
    case ProtoSchema.TUint32()                  => TInt()
    case ProtoSchema.TUint64()                  => TLong()
    case ProtoSchema.TSint32()                  => TInt()
    case ProtoSchema.TSint64()                  => TLong()
    case ProtoSchema.TFixed32()                 => TInt()
    case ProtoSchema.TFixed64()                 => TLong()
    case ProtoSchema.TSfixed32()                => TInt()
    case ProtoSchema.TSfixed64()                => TLong()
    case ProtoSchema.TBool()                    => TBoolean()
    case ProtoSchema.TString()                  => TString()
    case ProtoSchema.TBytes()                   => TByteArray()
    case ProtoSchema.TNamedType(name)           => TNamedType(name)
    case ProtoSchema.TOptional(value)           => TOption(value)
    case ProtoSchema.TRepeated(value)           => TList(value)
    case ProtoSchema.TRequired(value)           => TRequired(value)
    case ProtoSchema.TEnum(name, symbols, _, _) => TSum(name, symbols.map(_._1))
    case ProtoSchema.TMessage(name, fields, _)  => TProduct(name, fields.map(f => Field(f.name, f.tpe)))
  }

  def transformAvro[A]: Trans[AvroSchema, Schema, A] = Trans {
    case AvroSchema.TNull()          => TNull()
    case AvroSchema.TBoolean()       => TBoolean()
    case AvroSchema.TInt()           => TInt()
    case AvroSchema.TLong()          => TLong()
    case AvroSchema.TFloat()         => TFloat()
    case AvroSchema.TDouble()        => TDouble()
    case AvroSchema.TBytes()         => TByteArray()
    case AvroSchema.TString()        => TString()
    case AvroSchema.TNamedType(name) => TNamedType(name)
    case AvroSchema.TArray(item)     => TList(item)
    case AvroSchema.TMap(values)     => TMap(values)
    case AvroSchema.TRecord(name, _, _, _, fields) =>
      TProduct(name, fields.map(f => Field(f.name, f.tpe)))
    case AvroSchema.TEnum(name, _, _, _, symbols) => TSum(name, symbols)
    case AvroSchema.TUnion(options)               => TCoproduct(options)
    case AvroSchema.TFixed(_, _, _, _) =>
      ??? // I don't really know what to do with Fixed... https://avro.apache.org/docs/current/spec.html#Fixed
  }

}
