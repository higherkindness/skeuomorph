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

import higherkindness.skeuomorph.uast.types._
import higherkindness.skeuomorph.compdata.Ann
import higherkindness.droste._
import higherkindness.droste.syntax.embed._

object Transform {

  /**
   * transform Protobuf schema into Mu schema
   */
  def transformProto[A](implicit A: Embed[mu.Type, A]): Trans[protobuf.Type, mu.Type, A] = Trans {
    case protobuf.InjNull(_)                  => `null`[mu.Type, A]
    case protobuf.InjDouble(_)                => double[mu.Type, A]
    case protobuf.InjFloat(_)                 => float[mu.Type, A]
    case protobuf.InjInt32(_)                 => int[mu.Type, A]
    case protobuf.InjInt64(_)                 => long[mu.Type, A]
    case protobuf.InjUint32(_)                => int[mu.Type, A]
    case protobuf.InjUint64(_)                => long[mu.Type, A]
    case protobuf.InjSint32(_)                => int[mu.Type, A]
    case protobuf.InjSint64(_)                => long[mu.Type, A]
    case protobuf.InjFixed32(_)               => int[mu.Type, A]
    case protobuf.InjFixed64(_)               => long[mu.Type, A]
    case protobuf.InjSfixed32(_)              => int[mu.Type, A]
    case protobuf.InjSfixed64(_)              => long[mu.Type, A]
    case protobuf.InjBoolean(_)               => boolean[mu.Type, A]
    case protobuf.InjString(_)                => string[mu.Type, A]
    case protobuf.InjByteArray(_)             => byteArray[mu.Type, A]
    case protobuf.InjNamedType(TNamedType(n)) => option[mu.Type, A](namedType[mu.Type, A](n).embed)
    case protobuf.InjList(TList(value))       => list[mu.Type, A](value)
    case protobuf.InjOneOf(TOneOf(_, values)) =>
      union[mu.Type, A](values.map(FieldF.fieldType.get))
    case protobuf.InjMap(TMap(k, v)) => map[mu.Type, A](k, v)
    case protobuf.InjProtoEnum(Ann(TEnum(name, symbols), _)) =>
      enum[mu.Type, A](name, symbols)
    case protobuf.InjMessage(Ann(TRecord(name, fields), _)) =>
      record[mu.Type, A](name, fields)
    case protobuf.InjFileDescriptor(TFileDescriptor(values, _, _)) => containing[mu.Type, A](values)
  }

  def transformAvro[A]: TransM[Option, avro.Type, mu.Type, A] = TransM {
    case avro.InjNull(a)               => mu.InjNull(a).some
    case avro.InjBoolean(a)            => mu.InjBoolean(a).some
    case avro.InjInt(a)                => mu.InjInt(a).some
    case avro.InjLong(a)               => mu.InjLong(a).some
    case avro.InjFloat(a)              => mu.InjFloat(a).some
    case avro.InjDouble(a)             => mu.InjDouble(a).some
    case avro.InjByteArray(a)          => mu.InjByteArray(a).some
    case avro.InjString(a)             => mu.InjString(a).some
    case avro.InjMap(a)                => mu.InjMap(a).some
    case avro.InjAvroRecord(Ann(a, _)) => mu.InjRecord(a).some
    case avro.InjEnum(a)               => mu.InjEnum(a).some
    case avro.InjUnion(a)              => mu.InjUnion(a).some
    case avro.InjFixed(_)              => none[mu.Type[A]]
  }
}
