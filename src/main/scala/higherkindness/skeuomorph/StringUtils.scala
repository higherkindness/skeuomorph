package higherkindness.skeuomorph

object StringUtils {
  def decapitalize(s: String): String = if (s.isEmpty) s else s"${s.head.toLower}${s.tail}"
}
