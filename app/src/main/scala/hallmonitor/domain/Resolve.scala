package hallmonitor.domain

import hexis.{Answer, Confidence}

object Resolve:
  private enum Ruling:
    case Applies
    case DoesNotApply
    case Uncertain

  def apply(
      policy: Policy,
      face: ModelKind,
      answers: Map[CriterionId, Answer[?]],
      requested: Option[String],
      truncated: Boolean,
  ): Either[RouteError, Backend] =
    val index    = policy.criteria.iterator.map(c => c.id -> c).toMap
    val catalog  = policy.backends.filter(_.kind == face)
    val narrowed = policy.rules.foldLeft(catalog) {
      case (current, rule: Rule.Constraint) =>
        ruling(rule.when, face, index, answers, truncated) match
          case Some(Ruling.Applies) | Some(Ruling.Uncertain) =>
            current.filter(backend => rule.allow.contains(backend.id))
          case _ => current
      case (current, _) => current
    }
    val name = requested.map(_.trim).filter(value => value.nonEmpty && value != Names.Auto)
    name match
      case Some(wanted) =>
        catalog.find(backend => backend.id.value == wanted || backend.aliases.contains(wanted)) match
          case None                                                  => Left(RouteError.UnknownModel(wanted))
          case Some(backend) if !narrowed.exists(_.id == backend.id) =>
            Left(RouteError.ModelNotAllowed(wanted, narrowed.map(_.id)))
          case Some(backend) => Right(backend)
      case None if narrowed.isEmpty => Left(RouteError.NoEligibleBackend)
      case None => Right(rank(policy, narrowed, preferences(policy, face, index, answers, truncated)))
  end apply

  private def preferences(
      policy: Policy,
      face: ModelKind,
      index: Map[CriterionId, Criterion],
      answers: Map[CriterionId, Answer[?]],
      truncated: Boolean,
  ): List[Rule.Preference] =
    policy.rules.collect {
      case rule: Rule.Preference if ruling(rule.when, face, index, answers, truncated).contains(Ruling.Applies) =>
        rule
    }

  private def rank(policy: Policy, candidates: List[Backend], prefs: List[Rule.Preference]): Backend =
    val order = policy.backends.iterator.map(_.id).zipWithIndex.toMap
    candidates.minBy { backend =>
      val ruleScore = prefs.foldLeft(0) { (sum, pref) =>
        val at = pref.prefer.indexOf(backend.id)
        sum + (if at < 0 then pref.prefer.length else at)
      }
      val defaultAt    = policy.defaultPrefer.indexOf(backend.id)
      val defaultScore = if defaultAt < 0 then policy.defaultPrefer.length else defaultAt
      (ruleScore, defaultScore, order.getOrElse(backend.id, Int.MaxValue))
    }
  end rank

  private def ruling(
      when: List[Predicate],
      face: ModelKind,
      index: Map[CriterionId, Criterion],
      answers: Map[CriterionId, Answer[?]],
      truncated: Boolean,
  ): Option[Ruling] =
    if when.isEmpty then Some(Ruling.Applies)
    else
      val kept = when.filter(predicate => index.get(predicate.criterion).exists(_.faces.contains(face)))
      if kept.isEmpty then None
      else
        val judged = kept.map(predicate => judge(predicate, index(predicate.criterion), answers, truncated))
        Some(combine(judged))

  private def combine(rulings: List[Ruling]): Ruling =
    if rulings.exists(_ == Ruling.DoesNotApply) then Ruling.DoesNotApply
    else if rulings.exists(_ == Ruling.Uncertain) then Ruling.Uncertain
    else Ruling.Applies

  private def judge(
      predicate: Predicate,
      criterion: Criterion,
      answers: Map[CriterionId, Answer[?]],
      truncated: Boolean,
  ): Ruling =
    predicate match
      case Predicate.NoulYesAtLeast(_, atLeast) =>
        if truncated then Ruling.Uncertain
        else
          answers.get(criterion.id) match
            case Some(Answer.Noul(probability)) =>
              if asDouble(probability) >= asDouble(atLeast) then Ruling.Applies else Ruling.DoesNotApply
            case _ => Ruling.Uncertain
      case Predicate.ChoiceIs(_, options, minConfidence) =>
        answers.get(criterion.id) match
          case Some(choice: Answer.Choice[?]) =>
            if asDouble(choice.confidence) < asDouble(minConfidence) then Ruling.Uncertain
            else if options.contains(label(choice.choice)) then Ruling.Applies
            else Ruling.DoesNotApply
          case _ => Ruling.Uncertain
      case Predicate.ScoreAtLeast(_, level, minConfidence) =>
        score(criterion, answers, minConfidence) match
          case None      => Ruling.Uncertain
          case Some(idx) =>
            val target = scoreLevels(criterion).indexOf(level)
            if target < 0 then Ruling.Uncertain
            else if idx >= target then Ruling.Applies
            else Ruling.DoesNotApply
      case Predicate.ScoreAtMost(_, level, minConfidence) =>
        score(criterion, answers, minConfidence) match
          case None      => Ruling.Uncertain
          case Some(idx) =>
            val target = scoreLevels(criterion).indexOf(level)
            if target < 0 then Ruling.Uncertain
            else if idx <= target then Ruling.Applies
            else Ruling.DoesNotApply

  private def score(
      criterion: Criterion,
      answers: Map[CriterionId, Answer[?]],
      minConfidence: Confidence,
  ): Option[Int] =
    answers.get(criterion.id) match
      case Some(value: Answer.Score[?]) =>
        if asDouble(value.confidence) < asDouble(minConfidence) then None
        else
          val levels = scoreLevels(criterion)
          if levels.isEmpty then None
          else Some(math.round(value.score).toInt.max(0).min(levels.length - 1))
      case _ => None

  private def scoreLevels(criterion: Criterion): List[String] =
    criterion match
      case score: Criterion.Score => score.levels
      case _                      => Nil

  private def label(value: Any): String =
    value match
      case text: String => text
      case other        => String.valueOf(other)

  private def asDouble(value: Double): Double = value
end Resolve
