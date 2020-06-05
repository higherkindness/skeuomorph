/*
 * Copyright 2018-2020 47 Degrees <https://www.47deg.com>
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

import java.io.{File, FileOutputStream, InputStream}
import java.nio.file.{Files, Paths, StandardOpenOption}

import cats.effect.{Resource, Sync}

object FileUtils {
  def fileHandle[F[_]: Sync](name: String): Resource[F, File] =
    Resource.make(
      Sync[F].delay(new File(name))
    )(file => Sync[F].delay(file.deleteOnExit()))

  def fileOutputStream[F[_]: Sync](file: File): Resource[F, FileOutputStream] =
    Resource.make(
      Sync[F].delay(new FileOutputStream(file))
    )(fos => Sync[F].delay(fos.close()))

  def fileInputStream[F[_]: Sync](name: String): Resource[F, InputStream] =
    Resource.make(
      Sync[F].delay(Files.newInputStream(Paths.get(name), StandardOpenOption.DELETE_ON_CLOSE))
    )(is => Sync[F].delay(is.close()))
}
