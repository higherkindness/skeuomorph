import java.io.FileInputStream
import java.nio.file.{Path, Paths}
import cats.ApplicativeError
import com.github.os72.protocjar.Protoc
import com.google.protobuf.DescriptorProtos
import com.google.protobuf.DescriptorProtos.FileDescriptorProto

import scala.util.Try

trait Parser[F[_], I, O] {
  def parse(input: I)(implicit A: ApplicativeError[F, Throwable]): F[O]
}

// TODO: Error handling when proto file is not found
// TODO: Error handling when path to proto file is malformed.
// TODO: Stop breaking substitution principle!!!

object ParseProto {
  import scala.collection.JavaConverters._
  import org.apache.commons.compress.utils.IOUtils
  import java.io.FileOutputStream

  def apply[F[_],I,O](implicit parser: Parser[F, I, O]) = parser

  implicit def parseProto[F[_]] = new Parser[F, FileInputStream, Seq[FileDescriptorProto]] {
    override def parse(input: FileInputStream)(implicit A: ApplicativeError[F, Throwable]): F[Seq[FileDescriptorProto]] = {
      A.fromTry(Try(runProtoc(input)))
    }
  }

  def runProtoc(protoFileStream: FileInputStream): Seq[FileDescriptorProto] = {
    // Generate new file for protoc
    val fileOutput = new FileOutputStream("tmp")
    // Ohno! Ohno! Ohno! My kingdom for a Resource
    try {
      IOUtils.copy(protoFileStream, fileOutput)
      runProtoc("tmp", Paths.get("."))
    } finally {
      fileOutput.close()
      protoFileStream.close()
    }
  }

  def runProtoc(protoFileName: String, pathToProtoFile: Path): Seq[FileDescriptorProto] = {
    val descriptorFileName = s"$protoFileName.desc"

    Protoc.runProtoc(
      Array(
        "--include_imports",
        s"--descriptor_set_out=$descriptorFileName",
        s"--proto_path=${pathToProtoFile.toString}",
        protoFileName
      )
    )

    makeFileDescriptorProto(descriptorFileName)
  }

  private def makeFileDescriptorProto(descriptorFileName: String): Seq[FileDescriptorProto] = {
    // Note: This would really be better expressed as a cats-effect.Resource
    val fileInputStream: FileInputStream = new FileInputStream(descriptorFileName)
    try{
      val descriptorSet = DescriptorProtos.FileDescriptorSet.parseFrom(fileInputStream)
      descriptorSet.getFileList.asScala
    } finally {
      fileInputStream.close()
    }
  }

}

// TODO: Move to error contract folder/file
trait SkeuomorphError extends Exception {
  val message: String
}

case object ProtobufCompilationException extends SkeuomorphError {
  override val message: String = "Failed to compile protobuf file"

  override def getMessage = message
}

case object ProtobufParsingException extends SkeuomorphError {
  override val message = "Failed to parse input as protobuf file"

  override def getMessage = message
}



object Playground extends App {
  val result =
    ParseProto.runProtoc("sampleProto.proto", Paths.get("/Users/rebeccamark/sasquatch/skeuomorph/src/main/resources"))
  result.foreach { fileDescriptor =>
    println(fileDescriptor)
  }
}
