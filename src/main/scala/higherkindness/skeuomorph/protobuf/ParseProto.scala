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

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.os72.protocjar.Protoc
import higherkindness.skeuomorph.FileUtils._
import com.google.protobuf.DescriptorProtos.{FileDescriptorProto, FileDescriptorSet}
import higherkindness.skeuomorph.{Parser, _}
import scala.collection.JavaConverters._

object ParseProto {

  case class ProtoSource(filename: String, path: String)

  implicit def parseProto[F[_]]: Parser[F, ProtoSource, List[NativeDescriptor]] =
    new Parser[F, ProtoSource, List[NativeDescriptor]] {
      override def parse(input: ProtoSource)(implicit S: Sync[F]): F[List[NativeDescriptor]] =
        runProtoc(input)
    }

  private def runProtoc[F[_]: Sync](input: ProtoSource): F[List[NativeDescriptor]] = {
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
      fileDescriptor <- Sync[F].adaptError(makeFileDescriptor[F](descriptorFileName)) {
        case ex: Exception => ProtobufParsingException(ex)
      }
      nativeDescriptors <- Sync[F].adaptError(getNativeDescriptors[F](fileDescriptor)) {
        case ex: Exception => ProtobufNativeException(ex)
      }
    } yield nativeDescriptors
  }

  private def makeFileDescriptor[F[_]: Sync](descriptorFileName: String): F[FileDescriptorSet] =
    fileInputStream(descriptorFileName).use(fis => Sync[F].delay(FileDescriptorSet.parseFrom(fis)))

  private def getNativeDescriptors[F[_]: Sync](source: FileDescriptorSet): F[List[NativeDescriptor]] = {
    Sync[F].delay {
      val descriptors: List[FileDescriptorProto] = source.getFileList.asScala.toList
      descriptors.map(d => NativeDescriptor(d, descriptors))
    }
  }
}
