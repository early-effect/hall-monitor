package hallmonitor.docs

/** Install coordinate. Judge the raw build version. Do not trust a version after `-ci` was stripped. */
object DocsVersion:

  /** Last published tag. */
  val published = "0.1.0"

  /** A dynver distance (`-ci`, `+`, `SNAPSHOT`) is not a coordinate a reader should copy. */
  def advertise(raw: String): String =
    val trimmed  = raw.trim
    val distance = trimmed.contains("-ci") || trimmed.contains("+") || trimmed.contains("SNAPSHOT")
    if trimmed.nonEmpty && !distance then trimmed else published

  /** `specular.meta.version` is the raw build version. Absent in unit tests, so those use [[published]]. */
  def version: String =
    sys.props.get("specular.meta.version").map(advertise).getOrElse(published)
end DocsVersion
