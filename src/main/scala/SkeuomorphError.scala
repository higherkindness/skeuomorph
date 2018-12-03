trait SkeuomorphError extends Exception {
  val message: String
}

case object ProtobufCompilationException extends SkeuomorphError {
  override val message: String = "Protoc failed to compile protobuf file"

  override def getMessage = message
}

case object ProtobufParsingException extends SkeuomorphError {
  override val message = "Failed to parse input as protobuf file"

  override def getMessage = message
}
