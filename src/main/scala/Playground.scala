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

package higherkindness.skeuomorph

import java.io.FileInputStream

import cats.effect.IO
import scalapb.descriptors.{BaseDescriptor, FileDescriptor}

object Playground extends App {
  // An example of the contract Skeuomorph will support
  import higherkindness.skeuomorph.mu.{MuF, Transform}
  import higherkindness.skeuomorph.protobuf._
  import qq.droste.data.Mu
  import qq.droste.data.Mu._
  import qq.droste.scheme

  val readFile: IO[FileDescriptor] = ParseProto
    .parseProto[IO]
    .parse(new FileInputStream("/Users/rebeccamark/sasquatch/skeuomorph/src/main/resources/sampleProto.proto"))

  val fileDescriptor: FileDescriptor = readFile.unsafeRunSync()

  val parseProto: BaseDescriptor => Mu[ProtobufF] =
    scheme.ana(ProtobufF.fromProtobuf)

  val printProto: Mu[ProtobufF] => String =
    print.printSchema.print _

  val roundTrip: String = printProto(parseProto(fileDescriptor))

  // Render Proto file
  println(roundTrip)

  val transformToMu: FileDescriptor => Mu[MuF] =
    scheme.hylo(Transform.transformProto.algebra, ProtobufF.fromProtobuf)

  val printAsScala: Mu[MuF] => String =
    higherkindness.skeuomorph.mu.print.schema.print _

  // Render Scala
  println(printAsScala(transformToMu(fileDescriptor)))
}
