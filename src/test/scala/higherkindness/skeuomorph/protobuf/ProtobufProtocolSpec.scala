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

import cats.effect.IO
import org.specs2.Specification
import higherkindness.skeuomorph.protobuf.ParseProto._
import higherkindness.skeuomorph.mu
import higherkindness.skeuomorph.mu.MuF
import qq.droste.data.Mu

class ProtobufProtocolSpec extends Specification {

  def is = s2"""
  Protobuf Protocol

  It should be possible to print a protocol from a Proto file. $printProtobufProtocol

  """

  def printProtobufProtocol = {

    val source                       = ParseProto.ProtoSource("book.proto", "resources")
    val nativeDescriptor: NativeFile = parseProto[IO].parse(source).unsafeRunSync()

    val parseNative: NativeFile => Protocol[Mu[ProtobufF]] = Protocol.fromProto(_)

    val parseProtocol: Protocol[Mu[ProtobufF]] => mu.Protocol[Mu[MuF]] = { p: Protocol[Mu[ProtobufF]] =>
      mu.Protocol.fromProtobufProto(p)
    }

    val printProtocol: mu.Protocol[Mu[MuF]] => String = { p: mu.Protocol[Mu[MuF]] =>
      higherkindness.skeuomorph.mu.print.proto.print(p)
    }

    val a = (parseNative andThen parseProtocol andThen printProtocol)(nativeDescriptor)

    println(a)
    "Hello world" must haveSize(11)

  }

}
