/*
 * Copyright 2018-2020 47 Degrees Open Source <https://www.47deg.com>
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

import java.io.File

import higherkindness.droste._
import higherkindness.skeuomorph.Parser
import schema.OpenApi
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.either._

object ParseOpenApi {
  import JsonDecoders._

  case class YamlSource(file: File)
  case class JsonSource(file: File)

  implicit def parseYamlOpenApi[F[_], T](implicit T: Embed[JsonSchemaF, T]): Parser[F, YamlSource, OpenApi[T]] =
    new Parser[F, YamlSource, OpenApi[T]] {
      import yaml.{Decoder => _, _}
      override def parse(input: YamlSource)(implicit S: Sync[F]): F[OpenApi[T]] =
        readContent(input.file).flatMap(x =>
          S.fromEither(
            yaml
              .Decoder[OpenApi[T]]
              .apply(x)
              .left
              .map(_.valueOr(identity))
          )
        )
    }

  implicit def parseJsonOpenApi[F[_], T](implicit T: Embed[JsonSchemaF, T]): Parser[F, JsonSource, OpenApi[T]] =
    new Parser[F, JsonSource, OpenApi[T]] {
      import io.circe.Decoder
      import io.circe.parser

      override def parse(input: JsonSource)(implicit S: Sync[F]): F[OpenApi[T]] =
        for {
          content <- readContent(input.file)
          json    <- S.fromEither(parser.parse(content))
          openApi <- S.fromEither(Decoder[OpenApi[T]].decodeJson(json))
        } yield openApi
    }

  private def readContent[F[_]: Sync](file: File): F[String] = Sync[F].delay {
    scala.io.Source
      .fromFile(file)
      .getLines()
      .toList
      .mkString("\n")
  }

}
