import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Files

import cats.effect.{Resource, Sync}

object FileUtils {
  def fileHandle[F[_]: Sync](name: String): Resource[F, File] = Resource.make(
    Sync[F].delay(new File(name))
  )(
    file => Sync[F].delay(file.deleteOnExit())
  )

  def fileOutputStream[F[_]: Sync](file: File): Resource[F, FileOutputStream] = Resource.make(
    Sync[F].delay(new FileOutputStream(file))
  )(
    fos => Sync[F].delay(fos.close())
  )

  def fileInputStream[F[_]: Sync](name: String): Resource[F, FileInputStream] = Resource.make(
    Sync[F].delay(new FileInputStream(name))
  )(
    fis => {
      Sync[F].delay(fis.close())
      Sync[F].delay(Files.delete(new File(name).toPath)) // Think on this...
    }
  )
}
