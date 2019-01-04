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

import java.io.FileInputStream
import java.time.Instant

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.os72.protocjar.Protoc
import com.google.protobuf.descriptor.FileDescriptorSet
import higherkindness.skeuomorph.FileUtils._
import higherkindness.skeuomorph.{Parser, _}
import org.apache.commons.compress.utils.IOUtils
import scalapb.descriptors.FileDescriptor

object ParseProto {

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
    val protoCompilation = Sync[F].delay(
      Protoc.runProtoc(
        Array(
          "--include_imports",
          s"--descriptor_set_out=$descriptorFileName",
          s"--proto_path=$pathToProtoFile",
          protoFileName
        )
      )
    )

    for {
      _ <- Sync[F].ensure[Int](protoCompilation)(ProtobufCompilationException())((exitCode: Int) => exitCode == 0)
      fileDescriptor <- Sync[F].adaptError(makeFileDescriptor[F](descriptorFileName)) {
        case ex: Exception => ProtobufParsingException(ex)
      }
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
