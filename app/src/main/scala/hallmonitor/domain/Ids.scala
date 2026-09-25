package hallmonitor.domain

opaque type BackendId <: String = String

object BackendId:
  def apply(value: String): BackendId = value

  extension (id: BackendId) def value: String = id

opaque type CriterionId <: String = String

object CriterionId:
  def apply(value: String): CriterionId = value

  extension (id: CriterionId) def value: String = id

opaque type RuleId <: String = String

object RuleId:
  def apply(value: String): RuleId = value

  extension (id: RuleId) def value: String = id

opaque type Secret <: String = String

object Secret:
  def apply(value: String): Secret = value

  extension (secret: Secret) def reveal: String = secret

object Names:
  val Auto: String = "hall-monitor"
