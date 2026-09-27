package hallmonitor.domain

import hexis.{Answer, Confidence}

object Resolve:
  def apply(
      policy: Policy,
      face: ModelKind,
      answers: Map[CriterionId, Answer[?]],
      requested: Option[String],
      truncated: Boolean,
      unclassified: Boolean = false,
  ): RoutePlan =
    val index             = policy.criteria.iterator.map(c => c.id -> c).toMap
    val catalog           = policy.backends.filter(_.kind == face)
    val (narrowed, steps) =
      policy.rules.foldLeft((catalog, List.empty[Step])) { case ((current, acc), rule) =>
        val judged = judgeRule(rule.when, face, index, answers, truncated)
        rule match
          case constraint: Rule.Constraint =>
            val narrow    = judged == Ruling.Applies || judged == Ruling.Uncertain
            val remaining = if narrow then current.filter(backend => constraint.allow.contains(backend.id)) else current
            val step      = Step.Constraint(
              constraint.id,
              judged,
              narrow,
              constraint.allow.toList.sortBy(_.value),
              remaining.map(_.id),
            )
            (remaining, acc :+ step)
          case preference: Rule.Preference =>
            val step = Step.Preference(preference.id, judged, judged == Ruling.Applies, preference.prefer)
            (current, acc :+ step)
        end match
      }
    val prefs = policy.rules.collect {
      case rule: Rule.Preference if steps.collectFirst {
            case step: Step.Preference if step.id == rule.id && step.ranked => step
          }.isDefined =>
        rule
    }
    val name  = requested.map(_.trim).filter(value => value.nonEmpty && value != Names.Auto)
    val trace = Trace(name, answers, steps, narrowed.map(_.id), truncated, unclassified)
    name match
      case Some(wanted) =>
        catalog.find(backend => backend.id.value == wanted || backend.aliases.contains(wanted)) match
          case None =>
            plan(Nil, trace, Some(RouteError.UnknownModel(wanted)))
          case Some(backend) if !narrowed.exists(_.id == backend.id) =>
            plan(Nil, trace, Some(RouteError.ModelNotAllowed(wanted, narrowed.map(_.id))))
          case Some(backend) =>
            val rest = rank(policy, narrowed.filterNot(_.id == backend.id), prefs)
            plan(
              backend :: rest,
              trace.copy(steps = steps :+ Step.Pinned(wanted, backend.id) :+ Step.Ranked(rest.map(_.id))),
              None,
            )
      case None if narrowed.isEmpty =>
        plan(Nil, trace, Some(RouteError.NoEligibleBackend))
      case None =>
        val order = rank(policy, narrowed, prefs)
        plan(order, trace.copy(steps = steps :+ Step.Ranked(order.map(_.id))), None)
    end match
  end apply

  private def plan(order: List[Backend], trace: Trace, rejected: Option[RouteError]): RoutePlan =
    RoutePlan(order, trace, rejected)

  private def rank(policy: Policy, candidates: List[Backend], prefs: List[Rule.Preference]): List[Backend] =
    val order = policy.backends.iterator.map(_.id).zipWithIndex.toMap
    candidates.sortBy { backend =>
      val ruleScore = prefs.foldLeft(0) { (sum, pref) =>
        val at = pref.prefer.indexOf(backend.id)
        sum + (if at < 0 then pref.prefer.length else at)
      }
      val defaultAt    = policy.defaultPrefer.indexOf(backend.id)
      val defaultScore = if defaultAt < 0 then policy.defaultPrefer.length else defaultAt
      (ruleScore, defaultScore, order.getOrElse(backend.id, Int.MaxValue))
    }
  end rank

  private def judgeRule(
      when: List[Predicate],
      face: ModelKind,
      index: Map[CriterionId, Criterion],
      answers: Map[CriterionId, Answer[?]],
      truncated: Boolean,
  ): Ruling =
    if when.isEmpty then Ruling.Applies
    else
      val kept = when.filter(predicate => index.get(predicate.criterion).exists(_.faces.contains(face)))
      if kept.isEmpty then Ruling.NotOnThisFace
      else combine(kept.map(predicate => judge(predicate, index(predicate.criterion), answers, truncated)))

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
