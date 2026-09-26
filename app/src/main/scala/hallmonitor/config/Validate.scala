package hallmonitor.config

import hallmonitor.domain.*
import hexis.{Confidence, Probability}

object Validate:
  def apply(raw: AppRaw, env: String => Option[String]): Either[List[ConfigError], Loaded] =
    val errors                               = scala.collection.mutable.ListBuffer.empty[ConfigError]
    def note(error: ConfigError): Unit       = errors += error
    def notes(more: List[ConfigError]): Unit = errors ++= more

    if raw.maxStateChars <= 0 then note(ConfigError.BadLimit("maxStateChars"))
    if raw.upstreamIdleSeconds <= 0 then note(ConfigError.BadLimit("upstreamIdleSeconds"))
    if raw.listen.host.trim.isEmpty then note(ConfigError.EmptyId("listen host"))
    if raw.listen.port <= 0 || raw.listen.port > 65535 then note(ConfigError.BadLimit("listen.port"))

    val apiKey = secret("hall-monitor", raw.apiKey, raw.apiKeyEnv, env) match
      case Left(error)  => note(error); None
      case Right(value) => Some(value)

    val classifier = raw.classifier match
      case None        => None
      case Some(value) =>
        val base = value.baseUrl.trim.stripSuffix("/")
        if base.isEmpty then note(ConfigError.EmptyBaseUrl("classifier"))
        if value.model.trim.isEmpty then note(ConfigError.EmptyModel("classifier"))
        if value.timeoutSeconds <= 0 then note(ConfigError.BadTimeout)
        secret("classifier", value.apiKey, value.apiKeyEnv, env) match
          case Left(error) => note(error); None
          case Right(key)  =>
            Some(ClassifierEndpoint(base, value.model.trim, value.timeoutSeconds, key))

    val backends = raw.backends.zipWithIndex.flatMap { (backend, index) =>
      readBackend(backend, index, env) match
        case Left(found)  => notes(found); None
        case Right(value) => Some(value)
    }
    duplicates(backends.map(_.id.value), "backend", note)
    aliases(backends, note)

    val criteria = raw.criteria.flatMap { criterion =>
      readCriterion(criterion) match
        case Left(found)  => notes(found); None
        case Right(value) => Some(value)
    }
    duplicates(criteria.map(_.id.value), "criterion", note)

    if criteria.nonEmpty && raw.classifier.isEmpty then note(ConfigError.CriteriaWithoutClassifier)

    val byCriterion = criteria.iterator.map(c => c.id.value -> c).toMap
    val byBackend   = backends.iterator.map(b => b.id.value -> b).toMap
    val rules       = raw.rules.flatMap { rule =>
      readRule(rule, byCriterion, byBackend) match
        case Left(found)  => notes(found); None
        case Right(value) => Some(value)
    }
    duplicates(rules.map(_.id.value), "rule", note)

    val prefer = raw.defaultPrefer.flatMap { name =>
      if byBackend.contains(name) then Some(BackendId(name))
      else
        note(ConfigError.UnknownBackend("defaultPrefer", name))
        None
    }

    if errors.nonEmpty then Left(errors.toList)
    else
      Right(
        Loaded(
          policy = Policy(
            backends = backends,
            criteria = criteria,
            rules = rules,
            defaultPrefer = prefer,
            maxStateChars = raw.maxStateChars,
          ),
          apiKey = apiKey.getOrElse(Secret("")),
          classifier = classifier,
          listen = Listen(raw.listen.host.trim, raw.listen.port),
          upstreamIdleSeconds = raw.upstreamIdleSeconds,
        )
      )
    end if
  end apply

  private def readBackend(
      raw: BackendRaw,
      index: Int,
      env: String => Option[String],
  ): Either[List[ConfigError], Backend] =
    val owner = raw.id.trim match
      case "" => s"backend[$index]"
      case id => id
    val found = scala.collection.mutable.ListBuffer.empty[ConfigError]
    if raw.id.trim.isEmpty then found += ConfigError.EmptyId(owner)
    val kind = ModelKind.parse(raw.kind.trim) match
      case None        => found += ConfigError.BadKind(owner, raw.kind); None
      case Some(value) => Some(value)
    val base = raw.baseUrl.trim.stripSuffix("/")
    if base.isEmpty then found += ConfigError.EmptyBaseUrl(owner)
    if raw.upstreamModel.trim.isEmpty then found += ConfigError.EmptyModel(owner)
    val aliases = raw.aliases.map(_.trim)
    aliases.zipWithIndex.foreach { (alias, aliasIndex) =>
      if alias.isEmpty then found += ConfigError.EmptyId(s"$owner alias[$aliasIndex]")
    }
    val authName = raw.auth.map(_.trim).filter(_.nonEmpty).getOrElse("key")
    val hasKey   = raw.apiKey.exists(_.trim.nonEmpty) || raw.apiKeyEnv.exists(_.trim.nonEmpty)
    val auth     = authName match
      case "key" =>
        secret(owner, raw.apiKey, raw.apiKeyEnv, env) match
          case Left(error)  => found += error; None
          case Right(value) => Some(BackendAuth.Key(value))
      case "grok" =>
        if hasKey then found += ConfigError.AuthExtra(owner)
        Some(BackendAuth.Grok(raw.grokHome.map(_.trim).filter(_.nonEmpty).getOrElse("~/.grok")))
      case "bedrock" =>
        if hasKey then found += ConfigError.AuthExtra(owner)
        val profile = raw.profile.map(_.trim).filter(_.nonEmpty).getOrElse("us-dev")
        val region  = raw.region.map(_.trim).filter(_.nonEmpty).getOrElse("us-west-2")
        Some(BackendAuth.Bedrock(profile, region))
      case other =>
        found += ConfigError.BadAuth(owner, other)
        None
    val wire = raw.protocol.map(_.trim).filter(_.nonEmpty).getOrElse("chat") match
      case "chat" =>
        val field = raw.maxTokensField.map(_.trim).filter(_.nonEmpty) match
          case Some("max_tokens") | Some("max_completion_tokens") => raw.maxTokensField.map(_.trim)
          case Some(other)                                        =>
            found += ConfigError.BadMaxTokensField(owner, other)
            None
          case None => None
        Some(UpstreamWire.Chat(ChatCompat(rewriteDeveloper = raw.developerRole.contains(false), field)))
      case "responses" =>
        if kind.contains(ModelKind.Decision) then
          found += ConfigError.BadProtocol(owner, "responses on a decision backend")
        Some(UpstreamWire.Responses)
      case "messages" =>
        if kind.contains(ModelKind.Decision) then
          found += ConfigError.BadProtocol(owner, "messages on a decision backend")
        Some(UpstreamWire.Messages(raw.headers.getOrElse(Map.empty)))
      case other =>
        found += ConfigError.BadProtocol(owner, other)
        None
    if found.nonEmpty then Left(found.toList)
    else
      Right(
        Backend(
          BackendId(raw.id.trim),
          kind.get,
          base,
          raw.upstreamModel.trim,
          auth.get,
          aliases,
          wire.get,
        )
      )
    end if
  end readBackend

  private def readCriterion(raw: CriterionRaw): Either[List[ConfigError], Criterion] =
    val id    = raw.id.trim
    val owner = if id.isEmpty then "criterion" else s"criterion $id"
    val found = scala.collection.mutable.ListBuffer.empty[ConfigError]
    if id.isEmpty then found += ConfigError.EmptyId("criterion")
    val presentOptions = raw.options.filter(_.nonEmpty)
    val faces          = raw.faces match
      case None        => ModelKind.both
      case Some(names) =>
        if names.isEmpty then found += ConfigError.BadFace(owner, "")
        names.foldLeft(Set.empty[ModelKind]) { (acc, name) =>
          ModelKind.parse(name.trim) match
            case None        => found += ConfigError.BadFace(owner, name); acc
            case Some(value) => acc + value
        }
    val criterion = raw.kind.trim match
      case "noul" =>
        if presentOptions.isDefined then found += ConfigError.ForeignField(id, "options")
        if raw.levels.isDefined then found += ConfigError.ForeignField(id, "levels")
        (raw.yes.map(_.trim).filter(_.nonEmpty), raw.no.map(_.trim).filter(_.nonEmpty)) match
          case (Some(yes), Some(no)) => Some(Criterion.Noul(CriterionId(id), raw.instructions, yes, no, faces))
          case _                     =>
            if raw.yes.forall(_.trim.isEmpty) then found += ConfigError.EmptyId(s"$owner yes")
            if raw.no.forall(_.trim.isEmpty) then found += ConfigError.EmptyId(s"$owner no")
            None
      case "choice" =>
        if raw.yes.isDefined then found += ConfigError.ForeignField(id, "yes")
        if raw.no.isDefined then found += ConfigError.ForeignField(id, "no")
        if raw.levels.isDefined then found += ConfigError.ForeignField(id, "levels")
        presentOptions match
          case None =>
            found += ConfigError.ChoiceSize(id, 0)
            None
          case Some(options) =>
            if options.isEmpty || options.size > 255 then found += ConfigError.ChoiceSize(id, options.size)
            options.keys.foreach { key =>
              if key.trim.isEmpty then found += ConfigError.EmptyId(s"$owner option")
            }
            Some(Criterion.Choice(CriterionId(id), raw.instructions, options.toList, faces))
        end match
      case "score" =>
        if raw.yes.isDefined then found += ConfigError.ForeignField(id, "yes")
        if raw.no.isDefined then found += ConfigError.ForeignField(id, "no")
        if presentOptions.isDefined then found += ConfigError.ForeignField(id, "options")
        raw.levels match
          case None         => found += ConfigError.ScoreSize(id, 0); None
          case Some(levels) =>
            if levels.size < 2 || levels.size > 10 then found += ConfigError.ScoreSize(id, levels.size)
            levels.groupBy(identity).foreach { (level, copies) =>
              if copies.size > 1 then found += ConfigError.DuplicateLevel(id, level)
            }
            Some(Criterion.Score(CriterionId(id), raw.instructions, levels, faces))
      case other =>
        found += ConfigError.BadKind(owner, other)
        None
    if found.nonEmpty then Left(found.toList) else Right(criterion.get)
  end readCriterion

  private def readRule(
      raw: RuleRaw,
      criteria: Map[String, Criterion],
      backends: Map[String, Backend],
  ): Either[List[ConfigError], Rule] =
    val id    = raw.id.trim
    val owner = if id.isEmpty then "rule" else s"rule $id"
    val found = scala.collection.mutable.ListBuffer.empty[ConfigError]
    if id.isEmpty then found += ConfigError.EmptyId("rule")
    val predicates = raw.when.map(predicate => readPredicate(owner, predicate, criteria, found))
    raw.typeName.trim match
      case "constraint" =>
        if raw.prefer.isDefined then found += ConfigError.ForeignField(id, "prefer")
        val allow = raw.allow.getOrElse(Nil).map(_.trim)
        if allow.isEmpty then found += ConfigError.EmptyAllow(owner)
        allow.foreach { name =>
          if !backends.contains(name) then found += ConfigError.UnknownBackend(owner, name)
        }
        checkFaces(owner, predicates.flatten, allow, backends, criteria, constraint = true, found)
        if found.nonEmpty then Left(found.toList)
        else Right(Rule.Constraint(RuleId(id), predicates.flatten, allow.map(BackendId(_)).toSet))
      case "preference" =>
        if raw.allow.isDefined then found += ConfigError.ForeignField(id, "allow")
        val prefer = raw.prefer.getOrElse(Nil).map(_.trim)
        if prefer.isEmpty then found += ConfigError.EmptyPrefer(owner)
        prefer.foreach { name =>
          if !backends.contains(name) then found += ConfigError.UnknownBackend(owner, name)
        }
        checkFaces(owner, predicates.flatten, prefer, backends, criteria, constraint = false, found)
        if found.nonEmpty then Left(found.toList)
        else Right(Rule.Preference(RuleId(id), predicates.flatten, prefer.map(BackendId(_))))
      case other =>
        found += ConfigError.BadKind(owner, other)
        Left(found.toList)
    end match
  end readRule

  private def readPredicate(
      rule: String,
      raw: PredicateRaw,
      criteria: Map[String, Criterion],
      found: scala.collection.mutable.ListBuffer[ConfigError],
  ): Option[Predicate] =
    val name      = raw.criterion.trim
    val operators = List(raw.yesAtLeast.isDefined, raw.choice.isDefined, raw.atLeast.isDefined, raw.atMost.isDefined)
      .count(identity)
    if operators != 1 then
      found += ConfigError.PredicateOperators(rule, name)
      None
    else
      criteria.get(name) match
        case None =>
          found += ConfigError.UnknownCriterion(rule, name)
          None
        case Some(criterion) =>
          val id = CriterionId(name)
          raw.yesAtLeast match
            case Some(value) =>
              if raw.minConfidence.isDefined then found += ConfigError.ForeignField(name, "minConfidence")
              probability(rule, value, found) match
                case None          => None
                case Some(atLeast) =>
                  criterion match
                    case _: Criterion.Noul => Some(Predicate.NoulYesAtLeast(id, atLeast))
                    case _                 =>
                      found += ConfigError.ForeignField(name, "yesAtLeast")
                      None
            case None =>
              val minimum: Option[Confidence] = raw.minConfidence match
                case None        => Some(Confidence.unsafely(0.5))
                case Some(value) => this.confidence(rule, value, found)
              minimum.flatMap { min =>
                raw.choice match
                  case Some(options) =>
                    criterion match
                      case choice: Criterion.Choice =>
                        val known = choice.options.map(_._1).toSet
                        options.foreach { option =>
                          if !known.contains(option) then found += ConfigError.PredicateChoice(rule, option)
                        }
                        if options.isEmpty then found += ConfigError.PredicateChoice(rule, "")
                        Some(Predicate.ChoiceIs(id, options.toSet, min))
                      case _ =>
                        found += ConfigError.ForeignField(name, "choice")
                        None
                  case None =>
                    val level = raw.atLeast.orElse(raw.atMost).get
                    criterion match
                      case score: Criterion.Score =>
                        if !score.levels.contains(level) then found += ConfigError.PredicateLevel(rule, level)
                        if raw.atLeast.isDefined then Some(Predicate.ScoreAtLeast(id, level, min))
                        else Some(Predicate.ScoreAtMost(id, level, min))
                      case _ =>
                        found += ConfigError.ForeignField(name, "level")
                        None
              }
          end match
    end if
  end readPredicate

  private def checkFaces(
      rule: String,
      predicates: List[Predicate],
      names: List[String],
      backends: Map[String, Backend],
      criteria: Map[String, Criterion],
      constraint: Boolean,
      found: scala.collection.mutable.ListBuffer[ConfigError],
  ): Unit =
    val referenced = predicates.map(_.criterion.value).distinct
    referenced.foreach { name =>
      criteria.get(name).foreach { criterion =>
        val visible = names.exists(id => backends.get(id).exists(backend => criterion.faces.contains(backend.kind)))
        if !visible && names.nonEmpty then found += ConfigError.CriterionInvisible(rule, name)
      }
    }
    if constraint then
      val faces =
        if predicates.isEmpty then ModelKind.both
        else
          referenced.flatMap(name => criteria.get(name).map(_.faces)).reduceOption(_ intersect _).getOrElse(Set.empty)
      if predicates.nonEmpty && faces.isEmpty then found += ConfigError.ContradictoryFaces(rule)
      else
        faces.foreach { face =>
          val covered = names.exists(id => backends.get(id).exists(_.kind == face))
          if !covered then found += ConfigError.AllowMissesFace(rule, face.toString.toLowerCase)
        }
    end if
  end checkFaces

  private def probability(
      owner: String,
      value: Double,
      found: scala.collection.mutable.ListBuffer[ConfigError],
  ): Option[Probability] =
    if value >= 0.0 && value <= 1.0 then Some(Probability.unsafely(value))
    else
      found += ConfigError.BadProbability(owner, value)
      None

  private def confidence(
      owner: String,
      value: Double,
      found: scala.collection.mutable.ListBuffer[ConfigError],
  ): Option[Confidence] =
    if value >= 0.0 && value <= 1.0 then Some(Confidence.unsafely(value))
    else
      found += ConfigError.BadConfidence(owner, value)
      None

  private def duplicates(ids: List[String], kind: String, note: ConfigError => Unit): Unit =
    ids.groupBy(identity).foreach { (id, copies) =>
      if copies.size > 1 then note(ConfigError.DuplicateId(kind, id))
    }

  private def aliases(backends: List[Backend], note: ConfigError => Unit): Unit =
    val seen = scala.collection.mutable.Set.empty[String]
    backends.foreach { backend =>
      (backend.id.value :: backend.aliases).foreach { name =>
        if !seen.add(name) then note(ConfigError.AliasClash(name))
      }
    }

  private def secret(
      owner: String,
      apiKey: Option[String],
      apiKeyEnv: Option[String],
      env: String => Option[String],
  ): Either[ConfigError, Secret] =
    (apiKey.map(_.trim).filter(_.nonEmpty), apiKeyEnv.map(_.trim).filter(_.nonEmpty)) match
      case (Some(_), Some(_)) => Left(ConfigError.SecretBoth(owner))
      case (None, None)       =>
        if apiKey.exists(_.trim.isEmpty) then Left(ConfigError.SecretEmpty(owner))
        else Left(ConfigError.SecretNeither(owner))
      case (Some(value), None) => Right(Secret(value))
      case (None, Some(name))  =>
        env(name).map(_.trim).filter(_.nonEmpty) match
          case Some(value) => Right(Secret(value))
          case None        => Left(ConfigError.SecretUnset(owner, name))
end Validate
