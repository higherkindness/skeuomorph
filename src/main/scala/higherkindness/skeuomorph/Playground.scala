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

import cats.effect.IO

object Playground extends App {
  // An example of the contract Skeuomorph will support
  import higherkindness.skeuomorph.mu.MuF
  import higherkindness.skeuomorph.protobuf._
  import qq.droste.data.Mu
  import qq.droste.data.Mu._

  val readFile: IO[List[NativeDescriptor]] = ParseProto
    .parseProto[IO]
    .parse(ParseProto.ProtoSource("sampleProto.proto", "/Users/rafaparadela/code/47/skeuomorph/src/main/resources"))

  val nativeDescriptors: List[NativeDescriptor] = readFile.unsafeRunSync()

  val parseNative: NativeFile => Protocol[Mu[ProtobufF]] = { f: NativeFile =>
    Protocol.fromProto(f)
  }

  val parseProtocol: Protocol[Mu[ProtobufF]] => mu.Protocol[Mu[MuF]] = { p: Protocol[Mu[ProtobufF]] =>
    mu.Protocol.fromProtobufProto(p)
  }

  val printProtocol: mu.Protocol[Mu[MuF]] => String = { p: mu.Protocol[Mu[MuF]] =>
    higherkindness.skeuomorph.mu.print.proto.print(p)
  }

  val parseProto = parseNative andThen parseProtocol andThen printProtocol

  nativeDescriptors
    .collect {
      case f: NativeFile => parseProto(f)
    }
    .foreach(println)

}
