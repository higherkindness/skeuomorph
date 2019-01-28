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

import com.google.protobuf.descriptor.FileDescriptorProto
//import scalapb.descriptors.FileDescriptor
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.os72.protocjar.Protoc
import com.google.protobuf.descriptor.FileDescriptorSet
import higherkindness.skeuomorph.FileUtils._
import higherkindness.skeuomorph.{Parser, _}

object ParseProto {

  case class ProtoSource(filename: String, path: String)

  implicit def parseProto[F[_]]: Parser[F, ProtoSource, FileDescriptor] =
    new Parser[F, ProtoSource, FileDescriptor] {
      override def parse(input: ProtoSource)(implicit S: Sync[F]): F[FileDescriptor] =
        runProtoc(input)
    }

  private def runProtoc[F[_]: Sync](input: ProtoSource): F[FileDescriptor] = {
    val descriptorFileName = s"${input.filename}.desc"
    val protoCompilation: F[Int] = Sync[F].delay(
      Protoc.runProtoc(
        Array(
          "--plugin=protoc-gen-proto2_to_proto3",
          "--include_imports",
          s"--descriptor_set_out=${input.filename}.desc",
          s"--proto_path=${input.path}",
          input.filename
        )
      )
    )

    for {
      _ <- Sync[F].ensure[Int](protoCompilation)(ProtobufCompilationException())((exitCode: Int) => exitCode == 0)
      fileDescriptor <- Sync[F].adaptError(makeFileDescriptor[F](descriptorFileName, input.filename)) {
        case ex: Exception => ProtobufParsingException(ex)
      }
    } yield fileDescriptor
  }

  private def makeFileDescriptor[F[_]: Sync](descriptorFileName: String, protoFileName: String): F[FileDescriptorProto] =
    fileInputStream(descriptorFileName)
      .use { fis =>
        Sync[F].delay(FileDescriptorSet.parseFrom(fis).file)
      }
      .map { fileDescriptorProto =>
        val (descriptions, dependencies): (Seq[FileDescriptorProto], Seq[FileDescriptorProto]) =
          fileDescriptorProto.partition(_.name.fold(false)(_ == protoFileName))

        println("@@@@@@@@@@@@@@@@@@@@@@")
        fileDescriptorProto.foreach(println(_))

        println("£££££££££££££££££££££££")
        dependencies.foreach(println(_))

        val edited: Seq[FileDescriptor] = dependencies.map(FileDescriptor.buildFrom(_, Nil))
        println("£££££££££££££££££££££££")
        edited.foreach(f => println(f.messages))
        println("£££££££££££££££££££££££")
        edited.foreach(f => println(f.enums))
        println("£££££££££££££££££££££££")
        edited.foreach(f => println(f.asProto))

        val a: FileDescriptor =
          FileDescriptor.buildFrom(descriptions.head, dependencies.map(FileDescriptor.buildFrom(_, Nil)))

        println("********************")
        println(descriptions.head)
        println("********************")
        println(a.messages)
        println(a.enums)
        println(a.asProto)

        a
      }
}
