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
import com.google.protobuf.DescriptorProtos.{FileDescriptorProto, FileDescriptorSet}
import scala.collection.JavaConverters._
//import com.google.protobuf.descriptor.DescriptorProto.ReservedRange
//import com.google.protobuf.descriptor.FieldDescriptorProto.Label._
//import com.google.protobuf.descriptor.FieldDescriptorProto.Type._
//import higherkindness.skeuomorph.protobuf.ParseProto.ProtoSource
//import scalapb.descriptors.{BaseDescriptor, FileDescriptor}
//import protobuf.Optimize._
//import mu.Optimize._
//import scalapb.UnknownFieldSet

object Playground extends App {
  // An example of the contract Skeuomorph will support
//  import higherkindness.skeuomorph.mu.{MuF, Transform}
  import higherkindness.skeuomorph.protobuf._
  import qq.droste.data.Mu
  import qq.droste.data.Mu._
  import qq.droste.scheme

  val readFile: IO[FileDescriptorSet] = ParseProto
    .parseProto[IO]
    .parse(ParseProto.ProtoSource("sampleProto.proto", "/Users/rafaparadela/code/47/skeuomorph/src/main/resources"))

  val content: FileDescriptorSet             = readFile.unsafeRunSync()
  val descriptors: List[FileDescriptorProto] = content.getFileList.asScala.toList
  val nativeDescriptors: List[NativeDescriptor] =
    content.getFileList.asScala.toList.map(d => NativeDescriptor(d, descriptors))

  val parseProto: NativeDescriptor => Mu[ProtobufF] =
    scheme.ana(ProtobufF.fromProtobuf) // andThen optimizeProtobufF

  val printProto: Mu[ProtobufF] => String =
    print.printSchema.print _

  val roundTrip: List[String] = nativeDescriptors.map(d => printProto(parseProto(d)))

  // Render Proto file
  roundTrip.foreach(println)
//  val readFile: IO[FileDescriptorSet] = ParseProto
//    .parseProto[IO]
//    .parse(ProtoSource("sampleProto.proto", "/Users/rafaparadela/code/47/skeuomorph/src/main/resources"))
//
//  val fileDescriptor: FileDescriptor = readFile.unsafeRunSync()
//
//  // This step is new and is actually important for creating valid data
//  val optimizeProtobufF: Mu[ProtobufF] => Mu[ProtobufF] = repeatedTypes
//
//  val parseProto: BaseDescriptor => Mu[ProtobufF] =
//    scheme.ana(ProtobufF.fromProtobuf) andThen optimizeProtobufF
//
//  val printProto: Mu[ProtobufF] => String =
//    print.printSchema.print _
//
//  val roundTrip: String = printProto(parseProto(fileDescriptor))
//
//  // Render Proto file
////  println(roundTrip)
//
//  val protoToMu: Mu[ProtobufF] => Mu[MuF] =
//    scheme.cata(Transform.transformProto.algebra) andThen nestedNamedTypes andThen knownCoproductTypes
//
//  val transform: BaseDescriptor => Mu[MuF] = parseProto andThen protoToMu
//
//  val printAsScala: Mu[MuF] => String =
//    higherkindness.skeuomorph.mu.print.schema.print _
//
//  // Render Scala
////  println(printAsScala(transform(fileDescriptor)))

}
