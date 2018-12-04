import java.io.FileInputStream
import java.time.Instant

import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.{IO, Sync}
import com.github.os72.protocjar.Protoc
import scalapb.descriptors.{FileDescriptor => ScalaFileDescriptor}
import org.apache.commons.compress.utils.IOUtils
import FileUtils._

trait Parser[F[_], I, O] {
  def parse(input: I)(implicit S: Sync[F]): F[O]
}


object ParseProto {

  def apply[F[_],I,O](implicit parser: Parser[F, I, O]) = parser

  implicit def parseProto[F[_]]: Parser[F, FileInputStream, ScalaFileDescriptor] = new Parser[F, FileInputStream, ScalaFileDescriptor] {
    override def parse(input: FileInputStream)(implicit S: Sync[F]): F[ScalaFileDescriptor] = {
      transpile(input)
    }
  }

  private def transpile[F[_]: Sync](protoFileStream: FileInputStream): F[ScalaFileDescriptor] = {
    val tmpPathPrefix = "/tmp"
    val tmpFileName = s"$tmpPathPrefix/${Instant.now.toEpochMilli}.proto"

    fileHandle(tmpFileName)
      .flatMap(fileOutputStream[F])
      .use { fos =>
        for {
          _ <- Sync[F].delay(IOUtils.copy(protoFileStream, fos))
          fileDescriptor <- runProtoc(tmpFileName, tmpPathPrefix)
        } yield fileDescriptor
      }
  }

  private def runProtoc[F[_]: Sync](protoFileName: String, pathToProtoFile: String): F[ScalaFileDescriptor] = {
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

  private def makeFileDescriptor[F[_]: Sync](descriptorFileName: String): F[ScalaFileDescriptor] = {
    fileInputStream(descriptorFileName).use { fis: FileInputStream =>
      Sync[F].delay(com.google.protobuf.descriptor.FileDescriptorProto.parseFrom(fis))
      .map{fileDescriptorProto => ScalaFileDescriptor.buildFrom(fileDescriptorProto, Nil)}
    }
  }
}

object Playground extends App {

  // An example of the contract Skeuomorph will support
  val result = ParseProto.parseProto[IO].parse(new FileInputStream("/Users/rebeccamark/sasquatch/skeuomorph/src/main/resources/simpleProto.proto"))

  val t = result.unsafeRunSync()
  // Problem: The scala type has no messages. Parsing is broken
  println(t.messages)

}
