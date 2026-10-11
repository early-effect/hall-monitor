package hallmonitor.docs

/** Install coordinate. Only the last tag is a coordinate. A bare next version is not. */
object DocsVersion:

  /** Last published tag. */
  val published = "0.1.0"

  /** dynver-ci emits the next release with no suffix. That string is not on Central. */
  def advertise(raw: String): String =
    if raw.trim == published then published else published

  /** `specular.meta.version` is the raw build version. Absent in unit tests, so those use [[published]]. */
  def version: String =
    sys.props.get("specular.meta.version").map(advertise).getOrElse(published)
end DocsVersion
