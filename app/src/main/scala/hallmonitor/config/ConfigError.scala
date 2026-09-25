package hallmonitor.config

enum ConfigError:
  case DuplicateId(kind: String, id: String)
  case UnknownBackend(rule: String, id: String)
  case UnknownCriterion(rule: String, id: String)
  case EmptyAllow(rule: String)
  case EmptyPrefer(rule: String)
  case AllowMissesFace(rule: String, face: String)
  case CriterionInvisible(rule: String, criterion: String)
  case ChoiceSize(id: String, n: Int)
  case ScoreSize(id: String, n: Int)
  case BadProbability(owner: String, value: Double)
  case BadConfidence(owner: String, value: Double)
  case SecretBoth(owner: String)
  case SecretNeither(owner: String)
  case SecretEmpty(owner: String)
  case SecretUnset(owner: String, env: String)
  case CriteriaWithoutClassifier
  case PredicateOperators(rule: String, criterion: String)
  case PredicateLevel(rule: String, level: String)
  case PredicateChoice(rule: String, option: String)
  case ForeignField(id: String, field: String)
  case EmptyId(owner: String)
  case BadKind(owner: String, value: String)
  case BadFace(owner: String, value: String)
  case BadTimeout
  case BadLimit(field: String)
  case EmptyBaseUrl(owner: String)
  case EmptyModel(owner: String)
  case ContradictoryFaces(rule: String)
  case AliasClash(alias: String)
  case DuplicateLevel(id: String, level: String)

  def message: String =
    this match
      case DuplicateId(kind, id)               => s"duplicate $kind id $id"
      case UnknownBackend(rule, id)            => s"$rule references unknown backend $id"
      case UnknownCriterion(rule, id)          => s"$rule references unknown criterion $id"
      case EmptyAllow(rule)                    => s"$rule has an empty allow list"
      case EmptyPrefer(rule)                   => s"$rule has an empty prefer list"
      case AllowMissesFace(rule, face)         => s"$rule allow list has no $face backend"
      case CriterionInvisible(rule, criterion) =>
        s"$rule criterion $criterion is invisible to every backend it names"
      case ChoiceSize(id, n)                   => s"criterion $id choice options must be 1..255, got $n"
      case ScoreSize(id, n)                    => s"criterion $id score levels must be 2..10, got $n"
      case BadProbability(owner, value)        => s"$owner probability $value is outside [0, 1]"
      case BadConfidence(owner, value)         => s"$owner confidence $value is outside [0, 1]"
      case SecretBoth(owner)                   => s"$owner sets both apiKey and apiKeyEnv"
      case SecretNeither(owner)                => s"$owner sets neither apiKey nor apiKeyEnv"
      case SecretEmpty(owner)                  => s"$owner apiKey is empty"
      case SecretUnset(owner, env)             => s"$owner env $env is not set"
      case CriteriaWithoutClassifier           => "criteria require a classifier"
      case PredicateOperators(rule, criterion) =>
        s"$rule predicate $criterion must set exactly one of yesAtLeast, choice, atLeast, atMost"
      case PredicateLevel(rule, level)   => s"$rule level $level is not on the score criterion"
      case PredicateChoice(rule, option) => s"$rule choice $option is not on the criterion"
      case ForeignField(id, field)       => s"criterion $id cannot set $field"
      case EmptyId(owner)                => s"$owner id is empty"
      case BadKind(owner, value)         => s"$owner kind $value is not recognised"
      case BadFace(owner, value)         => s"$owner face $value is not recognised"
      case BadTimeout                    => "classifier timeoutSeconds must be positive"
      case BadLimit(field)               => s"$field must be positive"
      case EmptyBaseUrl(owner)           => s"$owner baseUrl is empty"
      case EmptyModel(owner)             => s"$owner model is empty"
      case ContradictoryFaces(rule)      => s"$rule criteria have no face in common"
      case AliasClash(alias)             => s"alias $alias collides with another name"
      case DuplicateLevel(id, level)     => s"criterion $id repeats level $level"
end ConfigError
