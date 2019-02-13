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
import org.scalacheck.Prop
import qq.droste._
import org.specs2.{ScalaCheck, Specification}

class ProtobufSpec extends Specification with ScalaCheck {

  import higherkindness.skeuomorph.instances._

  def is = s2"""
  Protobuf Schema

  It should be possible to create a ProtobufF from a NativeDescriptor. $convertNativeDescriptor

  """

  def convertNativeDescriptor = Prop.forAll { fileDescriptor: NativeFile =>
    val test = scheme.hylo(checkProto(fileDescriptor), ProtobufF.fromProtobuf)
    test(fileDescriptor)
  }

  def checkProto(desc: NativeDescriptor): Algebra[ProtobufF, Boolean] = Algebra {
    case ProtobufF.TOneOf(_, _)                => true
    case _: ProtobufF.TMap[Boolean]            => true
    case ProtobufF.TRepeated(_)                => true
    case e: ProtobufF.TEnum[Boolean]           => fieldTest(desc, Right(e))
    case m @ ProtobufF.TMessage(_, _, _)       => fieldTest(desc, Right(m))
    case f: ProtobufF.TFileDescriptor[Boolean] => f.values.forall(v => v)
    case ProtobufF.TNamedType(_)               => true
    case _                                     => true
  }

  def fieldTest(fieldDesc: NativeDescriptor, target: Either[Unit, ProtobufF[Boolean]]): Boolean = {
    fieldDesc match {
      case f: NativeFile =>
        f.values.forall(d => scheme.hylo(checkProto(d), ProtobufF.fromProtobuf)(implicitly[Functor[ProtobufF]])(d))
      case d: NativeMessage =>
        target match {
          case Right(m: ProtobufF.TMessage[Boolean]) =>
            m.name == d.name &&
              d.fields.forall(f =>
                scheme.hylo(checkProto(f.tpe), ProtobufF.fromProtobuf)(implicitly[Functor[ProtobufF]])(f.tpe))
          case _ => true
        }
      case e: NativeEnum =>
        target match {
          case Right(tEnum: ProtobufF.TEnum[Boolean]) =>
            e.symbols.length == tEnum.symbols.length && tEnum.symbols.head._2 == 0
          case _ => false
        }
      case _ => true
    }
  }
}
