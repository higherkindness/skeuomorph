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

trait SkeuomorphError extends Exception {
  val message: String
}

case class ProtobufCompilationException(originalError: Exception) extends SkeuomorphError {

  override val message: String = s"Protoc failed to compile protobuf file with error: ${originalError.getMessage}"

  override def getMessage: String = message
}

case class ProtobufParsingException(originalError: Exception) extends SkeuomorphError {
  override val message = s"Failed to parse compiled proto file with error: ${originalError.getMessage}"

  override def getMessage: String = message
}
