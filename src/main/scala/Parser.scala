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

import java.io.FileInputStream
import java.time.Instant

import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.{IO, Sync}
import com.github.os72.protocjar.Protoc
import scalapb.descriptors.FileDescriptor
import com.google.protobuf.descriptor.FileDescriptorSet
import org.apache.commons.compress.utils.IOUtils
import FileUtils._
import skeuomorph.protobuf.ProtobufF

trait Parser[F[_], I, O] {
  def parse(input: I)(implicit S: Sync[F]): F[O]
}

object ParseProto {

  def apply[F[_], I, O](implicit parser: Parser[F, I, O]) = parser

  implicit def parseProto[F[_]]: Parser[F, FileInputStream, FileDescriptor] =
    new Parser[F, FileInputStream, FileDescriptor] {
      override def parse(input: FileInputStream)(implicit S: Sync[F]): F[FileDescriptor] =
        transpile(input)
    }

  private def transpile[F[_]: Sync](protoFileStream: FileInputStream): F[FileDescriptor] = {
    val tmpPathPrefix = "/tmp"
    val tmpFileName   = s"$tmpPathPrefix/${Instant.now.toEpochMilli}.proto"

    fileHandle(tmpFileName)
      .flatMap(fileOutputStream[F])
      .use { fos =>
        for {
          _              <- Sync[F].delay(IOUtils.copy(protoFileStream, fos))
          fileDescriptor <- runProtoc(tmpFileName, tmpPathPrefix)
        } yield fileDescriptor
      }
  }

  private def runProtoc[F[_]: Sync](protoFileName: String, pathToProtoFile: String): F[FileDescriptor] = {
    val descriptorFileName = s"$protoFileName.desc"

    for {
      _ <- Sync[F].delay(
        Protoc.runProtoc(
          Array(
            "--include_imports",
            s"--descriptor_set_out=$descriptorFileName",
            s"--proto_path=$pathToProtoFile",
            protoFileName
          )
        )
      )
      fileDescriptor <- makeFileDescriptor[F](descriptorFileName)
    } yield fileDescriptor
  }

  private def makeFileDescriptor[F[_]: Sync](descriptorFileName: String): F[FileDescriptor] =
    fileInputStream(descriptorFileName)
      .use { fis =>
        for {
          scalaFileDescriptorSet <- Sync[F].delay(FileDescriptorSet.parseFrom(fis))
          fileDescProto = scalaFileDescriptorSet.file.head // Is there a condition under which there would be more than one?
        } yield fileDescProto
      }
      .map { fileDescriptorProto =>
        FileDescriptor.buildFrom(fileDescriptorProto, Nil)
      }
}

object Playground extends App {
  // An example of the contract Skeuomorph will support
  val result = ParseProto
    .parseProto[IO]
    .parse(new FileInputStream("/Users/rebeccamark/sasquatch/skeuomorph/src/main/resources/sampleProto.proto"))

  val fileDescriptor: FileDescriptor = result.unsafeRunSync()

  val value = ProtobufF.fromProtobuf(fileDescriptor)

  pprint.pprintln(value)

}
