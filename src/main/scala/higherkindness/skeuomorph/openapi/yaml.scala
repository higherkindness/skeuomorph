/*
 * Copyright 2018-2021 47 Degrees Open Source <https://www.47deg.com>
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

package higherkindness.skeuomorph.openapi
import io.circe.{Decoder => JsonDecoder, _}
import io.circe.yaml.parser
import cats.syntax.all._

object yaml {
  type Failure    = Either[ParsingFailure, DecodingFailure]
  type Decoder[A] = String => Either[Failure, A]

  object Decoder {
    def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder
  }
  implicit def fromJsonDecoder[A: JsonDecoder]: Decoder[A] =
    parser.parse(_).leftMap(_.asLeft).flatMap(JsonDecoder[A].decodeJson(_).leftMap(_.asRight))

}
