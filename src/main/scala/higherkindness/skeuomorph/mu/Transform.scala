/*
 * Copyright 2018-2020 47 Degrees Open Source <https://www.47deg.com>
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
import higherkindness.skeuomorph.protobuf.{FixedWidth, ProtobufF, Signed, Unsigned}
import higherkindness.droste.{Embed, Trans}

object Transform {

  import MuF._

  /**
   * transform Protobuf schema into Mu schema
   */
  def transformProto[A](implicit A: Embed[MuF, A]): Trans[ProtobufF, MuF, A] =
    Trans {
      case ProtobufF.TNull()                          => TNull()
      case ProtobufF.TDouble()                        => TDouble()
      case ProtobufF.TFloat()                         => TFloat()
      case ProtobufF.TInt32()                         => pbInt()
      case ProtobufF.TInt64()                         => pbLong()
      case ProtobufF.TUint32()                        => pbInt(Unsigned)
      case ProtobufF.TUint64()                        => pbLong(Unsigned)
      case ProtobufF.TSint32()                        => pbInt(Signed)
      case ProtobufF.TSint64()                        => pbLong(Signed)
      case ProtobufF.TFixed32()                       => pbInt(FixedWidth)
      case ProtobufF.TFixed64()                       => pbLong(FixedWidth)
      case ProtobufF.TSfixed32()                      => pbInt(FixedWidth, Signed)
      case ProtobufF.TSfixed64()                      => pbLong(FixedWidth, Signed)
      case ProtobufF.TBool()                          => TBoolean()
      case ProtobufF.TString()                        => TString()
      case ProtobufF.TBytes()                         => TByteArray(Length.Arbitrary)
      case ProtobufF.TNamedType(prefix, name)         => TNamedType(prefix, name)
      case ProtobufF.TOptionalNamedType(prefix, name) => TOption(A.algebra(TNamedType(prefix, name)))
      case ProtobufF.TRepeated(value)                 => TList(value)
      case ProtobufF.TEnum(name, symbols, _, _)       => TSum(name, symbols.map(SumField.tupled))
      case ProtobufF.TMessage(name, fields, _, nestedMessages, nestedEnums) =>
        TProduct(name, None, fields.map(f => Field(f.name, f.tpe, Some(f.indices))), nestedMessages, nestedEnums)
      case ProtobufF.TFileDescriptor(values, _, _) => TContaining(values)
      case ProtobufF.TOneOf(_, fields)             => TOption(A.algebra(TCoproduct(fields.map(_.tpe))))
      case ProtobufF.TMap(key, values)             => TMap(Some(key), values)
    }

  def transformAvro[A]: Trans[AvroF, MuF, A] =
    Trans {
      case AvroF.TNull()                     => TNull()
      case AvroF.TBoolean()                  => TBoolean()
      case AvroF.TInt()                      => int()
      case AvroF.TLong()                     => long()
      case AvroF.TFloat()                    => TFloat()
      case AvroF.TDouble()                   => TDouble()
      case AvroF.TBytes()                    => TByteArray(Length.Arbitrary)
      case AvroF.TString()                   => TString()
      case AvroF.TNamedType(namespace, name) => TNamedType(namespace.split('.').toList, name)
      case AvroF.TArray(item)                => TList(item)
      case AvroF.TMap(values)                => TMap(None, values)
      case AvroF.TRecord(name, namespace, _, _, fields) =>
        val muFields = fields.map(f => Field(f.name, f.tpe, indices = None))
        TProduct(name, namespace, muFields, Nil, Nil)
      case AvroF.TEnum(name, _, _, _, symbols) => TSum(name, symbols.zipWithIndex.map(SumField.tupled))
      case AvroF.TUnion(options)               => TCoproduct(options)
      case AvroF.TFixed(_, _, _, l) => TByteArray(Length.Fixed(l))
      case AvroF.TDate()                    => TDate()
      case AvroF.TTimestampMillis()         => TInstant()
      case AvroF.TUUID()                    => TUUID()
      case AvroF.TDecimal(precision, scale) => TDecimal(precision, scale)
    }

}
