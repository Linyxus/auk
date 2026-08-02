package auk.loop

import auk.llm.tools.Json
import auk.llm.tools.Json.get

/** JSON for [[LoopEvent]], one compact object per ledger line.
  *
  * Strict in both directions, because a ledger is the loop's only memory: an
  * unknown `type`, a missing field, or a field of the wrong kind is an error
  * rather than something to shrug off with a default. Errors name the field by
  * path (`attempt_submitted.snapshotCommits`), so a complaint about a ledger line
  * points at the exact thing that is wrong with it.
  *
  * Every case round-trips exactly. Two shapes are worth knowing about:
  *
  *   - Optional fields are *omitted* when `None` rather than written as `null`, so
  *     a `None` decodes back to `None`. An explicit `null` is read as absent too,
  *     which costs nothing and keeps a hand-edited ledger readable.
  *   - `Map[String, Double]` metrics are written with their keys sorted, so the
  *     line is stable and diffable regardless of how the map was built. The map
  *     itself round-trips; the field order within the JSON object does not
  *     survive as-written, which nothing depends on. A whole-valued metric is
  *     written as `2` rather than `2.0` — the same `Double` comes back, but it is
  *     worth knowing before squinting at a ledger.
  *
  * Non-finite metrics (a checker dividing by zero) have no JSON number, so they
  * are written as the strings `"NaN"`, `"Infinity"` and `"-Infinity"` and read
  * back as the doubles they name. Without this an appended event could throw
  * where it should merely be ugly.
  */
object LoopCodec:
  /** Encode an event as one compact JSON line for the ledger. */
  def encode(event: LoopEvent): String = toJson(event).render

  /** Decode one ledger line, or describe why it is not an event. */
  def decode(line: String): Either[String, LoopEvent] =
    Json.parse(line).flatMap(fromJson)

  // --- encoding ---------------------------------------------------------------

  def toJson(event: LoopEvent): Json =
    import LoopEvent.*
    val tag = "type" -> Json.Str(event.kind)
    event match
      case LoopCreated(loopId, baselineCommit, headAtCreation, sessionId, at) =>
        Json.Obj(List(
          tag,
          "loopId"         -> Json.Str(loopId),
          "baselineCommit" -> Json.Str(baselineCommit),
          "headAtCreation" -> Json.Str(headAtCreation),
          "sessionId"      -> Json.Str(sessionId),
          "at"             -> Json.Str(at)
        ))

      case DefAttached(version, source, goal, rubric, budgets, artifactSchema, at) =>
        Json.Obj(List(
          tag,
          "version"        -> Json.num(version),
          "source"         -> Json.Str(source),
          "goal"           -> Json.Str(goal),
          "rubric"         -> Json.Str(rubric),
          "budgets"        -> encodeBudgets(budgets),
          "artifactSchema" -> artifactSchema,
          "at"             -> Json.Str(at)
        ))

      case ConfigAmended(goal, rubric, budgets, at) =>
        Json.Obj(
          (tag :: Nil)
            ++ goal.map(g => "goal" -> Json.Str(g))
            ++ rubric.map(r => "rubric" -> Json.Str(r))
            ++ budgets.map(b => "budgets" -> encodeBudgets(b))
            ++ List("at" -> Json.Str(at))
        )

      case GenerationStarted(gen, parent, sessionId, at) =>
        Json.Obj(
          List(tag, "gen" -> Json.num(gen))
            ++ parent.map(p => "parent" -> Json.num(p))
            ++ List("sessionId" -> Json.Str(sessionId), "at" -> Json.Str(at))
        )

      case AttemptSubmitted(gen, attempt, artifact, description, knowledge, snapshotCommits, at) =>
        Json.Obj(
          List(
            tag,
            "gen"         -> Json.num(gen),
            "attempt"     -> Json.num(attempt),
            "artifact"    -> artifact,
            "description" -> Json.Str(description)
          )
            ++ knowledge.map(k => "knowledge" -> Json.Str(k))
            ++ List(
              "snapshotCommits" -> Json.Arr(snapshotCommits.map(Json.Str(_))),
              "at"              -> Json.Str(at)
            )
        )

      case CheckCompleted(gen, attempt, pass, reasons, metrics, at) =>
        Json.Obj(List(
          tag,
          "gen"     -> Json.num(gen),
          "attempt" -> Json.num(attempt),
          "pass"    -> Json.Bool(pass),
          "reasons" -> Json.Arr(reasons.map(Json.Str(_))),
          "metrics" -> encodeMetrics(metrics),
          "at"      -> Json.Str(at)
        ))

      case VerdictIssued(gen, attempt, accepted, feedback, goalReached, at) =>
        Json.Obj(List(
          tag,
          "gen"         -> Json.num(gen),
          "attempt"     -> Json.num(attempt),
          "accepted"    -> Json.Bool(accepted),
          "feedback"    -> Json.Str(feedback),
          "goalReached" -> Json.Bool(goalReached),
          "at"          -> Json.Str(at)
        ))

      case GenerationAccepted(gen, snapshotId, commit, description, metrics, at) =>
        Json.Obj(List(
          tag,
          "gen"         -> Json.num(gen),
          "snapshotId"  -> Json.Str(snapshotId),
          "commit"      -> Json.Str(commit),
          "description" -> Json.Str(description),
          "metrics"     -> encodeMetrics(metrics),
          "at"          -> Json.Str(at)
        ))

      case GenerationAbandoned(gen, attempts, rescueSnapshotId, at) =>
        Json.Obj(
          List(tag, "gen" -> Json.num(gen), "attempts" -> Json.num(attempts))
            ++ rescueSnapshotId.map(id => "rescueSnapshotId" -> Json.Str(id))
            ++ List("at" -> Json.Str(at))
        )

      case ExternalEditsAdopted(snapshotId, commit, sessionId, at) =>
        Json.Obj(List(
          tag,
          "snapshotId" -> Json.Str(snapshotId),
          "commit"     -> Json.Str(commit),
          "sessionId"  -> Json.Str(sessionId),
          "at"         -> Json.Str(at)
        ))

      case Parked(reason, at) =>
        Json.Obj(List(tag, "reason" -> encodeReason(reason), "at" -> Json.Str(at)))

      case Resumed(sessionId, at) =>
        Json.Obj(List(tag, "sessionId" -> Json.Str(sessionId), "at" -> Json.Str(at)))

  private def encodeBudgets(b: Budgets): Json =
    Json.Obj(List(
      "maxGenerations"           -> Json.num(b.maxGenerations),
      "patience"                 -> Json.num(b.patience),
      "maxAttemptsPerGeneration" -> Json.num(b.maxAttemptsPerGeneration)
    ))

  private def encodeReason(reason: ParkReason): Json = reason match
    case ParkReason.GoalReached       => Json.Obj(List("kind" -> Json.Str("goal_reached")))
    case ParkReason.BudgetExhausted   => Json.Obj(List("kind" -> Json.Str("budget_exhausted")))
    case ParkReason.PatienceExhausted => Json.Obj(List("kind" -> Json.Str("patience_exhausted")))
    case ParkReason.UserRequested     => Json.Obj(List("kind" -> Json.Str("user_requested")))
    case ParkReason.ApiFailure(detail) =>
      Json.Obj(List("kind" -> Json.Str("api_failure"), "detail" -> Json.Str(detail)))
    case ParkReason.Anomaly(detail) =>
      Json.Obj(List("kind" -> Json.Str("anomaly"), "detail" -> Json.Str(detail)))

  private def encodeMetrics(metrics: Map[String, Double]): Json =
    Json.Obj(metrics.toList.sortBy((key, _) => key).map((key, value) => key -> encodeMetric(value)))

  private def encodeMetric(value: Double): Json =
    if value.isNaN then Json.Str("NaN")
    else if value == Double.PositiveInfinity then Json.Str("Infinity")
    else if value == Double.NegativeInfinity then Json.Str("-Infinity")
    else Json.num(value)

  // --- decoding ---------------------------------------------------------------

  def fromJson(json: Json): Either[String, LoopEvent] =
    import LoopEvent.*
    json match
      case obj: Json.Obj =>
        obj.get("type") match
          case Some(Json.Str(tag)) =>
            tag match
              case "loop_created" =>
                for
                  loopId         <- str(obj, tag, "loopId")
                  baselineCommit <- str(obj, tag, "baselineCommit")
                  headAtCreation <- str(obj, tag, "headAtCreation")
                  sessionId      <- str(obj, tag, "sessionId")
                  at             <- str(obj, tag, "at")
                yield LoopCreated(loopId, baselineCommit, headAtCreation, sessionId, at)

              case "def_attached" =>
                for
                  version        <- int(obj, tag, "version")
                  source         <- str(obj, tag, "source")
                  goal           <- str(obj, tag, "goal")
                  rubric         <- str(obj, tag, "rubric")
                  budgets        <- budgetsAt(obj, tag, "budgets")
                  artifactSchema <- raw(obj, tag, "artifactSchema")
                  at             <- str(obj, tag, "at")
                yield DefAttached(version, source, goal, rubric, budgets, artifactSchema, at)

              case "config_amended" =>
                for
                  goal    <- optStr(obj, tag, "goal")
                  rubric  <- optStr(obj, tag, "rubric")
                  budgets <- optBudgets(obj, tag, "budgets")
                  at      <- str(obj, tag, "at")
                yield ConfigAmended(goal, rubric, budgets, at)

              case "generation_started" =>
                for
                  gen       <- int(obj, tag, "gen")
                  parent    <- optInt(obj, tag, "parent")
                  sessionId <- str(obj, tag, "sessionId")
                  at        <- str(obj, tag, "at")
                yield GenerationStarted(gen, parent, sessionId, at)

              case "attempt_submitted" =>
                for
                  gen             <- int(obj, tag, "gen")
                  attempt         <- int(obj, tag, "attempt")
                  artifact        <- raw(obj, tag, "artifact")
                  description     <- str(obj, tag, "description")
                  knowledge       <- optStr(obj, tag, "knowledge")
                  snapshotCommits <- strList(obj, tag, "snapshotCommits")
                  at              <- str(obj, tag, "at")
                yield AttemptSubmitted(gen, attempt, artifact, description, knowledge, snapshotCommits, at)

              case "check_completed" =>
                for
                  gen     <- int(obj, tag, "gen")
                  attempt <- int(obj, tag, "attempt")
                  pass    <- bool(obj, tag, "pass")
                  reasons <- strList(obj, tag, "reasons")
                  metrics <- metricsAt(obj, tag, "metrics")
                  at      <- str(obj, tag, "at")
                yield CheckCompleted(gen, attempt, pass, reasons, metrics, at)

              case "verdict_issued" =>
                for
                  gen         <- int(obj, tag, "gen")
                  attempt     <- int(obj, tag, "attempt")
                  accepted    <- bool(obj, tag, "accepted")
                  feedback    <- str(obj, tag, "feedback")
                  goalReached <- bool(obj, tag, "goalReached")
                  at          <- str(obj, tag, "at")
                yield VerdictIssued(gen, attempt, accepted, feedback, goalReached, at)

              case "generation_accepted" =>
                for
                  gen         <- int(obj, tag, "gen")
                  snapshotId  <- str(obj, tag, "snapshotId")
                  commit      <- str(obj, tag, "commit")
                  description <- str(obj, tag, "description")
                  metrics     <- metricsAt(obj, tag, "metrics")
                  at          <- str(obj, tag, "at")
                yield GenerationAccepted(gen, snapshotId, commit, description, metrics, at)

              case "generation_abandoned" =>
                for
                  gen      <- int(obj, tag, "gen")
                  attempts <- int(obj, tag, "attempts")
                  rescue   <- optStr(obj, tag, "rescueSnapshotId")
                  at       <- str(obj, tag, "at")
                yield GenerationAbandoned(gen, attempts, rescue, at)

              case "external_edits_adopted" =>
                for
                  snapshotId <- str(obj, tag, "snapshotId")
                  commit     <- str(obj, tag, "commit")
                  sessionId  <- str(obj, tag, "sessionId")
                  at         <- str(obj, tag, "at")
                yield ExternalEditsAdopted(snapshotId, commit, sessionId, at)

              case "parked" =>
                for
                  reason <- reasonAt(obj, tag, "reason")
                  at     <- str(obj, tag, "at")
                yield Parked(reason, at)

              case "resumed" =>
                for
                  sessionId <- str(obj, tag, "sessionId")
                  at        <- str(obj, tag, "at")
                yield Resumed(sessionId, at)

              case other => Left(s"type: unknown loop event type '$other'")
          case Some(other) => Left(s"type: expected a string but found ${other.typeName}")
          case None        => Left("type: missing field")
      case other => Left(s"a loop event must be an object but was ${other.typeName}")

  private def budgetsAt(obj: Json.Obj, path: String, key: String): Either[String, Budgets] =
    field(obj, path, key).flatMap(decodeBudgets(_, s"$path.$key"))

  private def optBudgets(obj: Json.Obj, path: String, key: String): Either[String, Option[Budgets]] =
    present(obj, key) match
      case None       => Right(None)
      case Some(json) => decodeBudgets(json, s"$path.$key").map(Some(_))

  private def decodeBudgets(json: Json, path: String): Either[String, Budgets] = json match
    case obj: Json.Obj =>
      for
        maxGenerations <- int(obj, path, "maxGenerations")
        patience       <- int(obj, path, "patience")
        maxAttempts    <- int(obj, path, "maxAttemptsPerGeneration")
      yield Budgets(maxGenerations, patience, maxAttempts)
    case other => Left(s"$path: expected an object but found ${other.typeName}")

  private def reasonAt(obj: Json.Obj, path: String, key: String): Either[String, ParkReason] =
    field(obj, path, key).flatMap: json =>
      val reasonPath = s"$path.$key"
      json match
        case reason: Json.Obj =>
          str(reason, reasonPath, "kind").flatMap:
            case "goal_reached"       => Right(ParkReason.GoalReached)
            case "budget_exhausted"   => Right(ParkReason.BudgetExhausted)
            case "patience_exhausted" => Right(ParkReason.PatienceExhausted)
            case "user_requested"     => Right(ParkReason.UserRequested)
            case "api_failure"        => str(reason, reasonPath, "detail").map(ParkReason.ApiFailure(_))
            case "anomaly"            => str(reason, reasonPath, "detail").map(ParkReason.Anomaly(_))
            case other                => Left(s"$reasonPath.kind: unknown park reason '$other'")
        case other => Left(s"$reasonPath: expected an object but found ${other.typeName}")

  private def metricsAt(obj: Json.Obj, path: String, key: String): Either[String, Map[String, Double]] =
    field(obj, path, key).flatMap:
      case Json.Obj(fields) =>
        traverse(fields)((name, value) => decodeMetric(value, s"$path.$key.$name").map(name -> _)).map(_.toMap)
      case other => Left(s"$path.$key: expected an object but found ${other.typeName}")

  private def decodeMetric(json: Json, path: String): Either[String, Double] = json match
    case Json.Num(n)            => Right(n.toDouble)
    case Json.Str("NaN")        => Right(Double.NaN)
    case Json.Str("Infinity")   => Right(Double.PositiveInfinity)
    case Json.Str("-Infinity")  => Right(Double.NegativeInfinity)
    case other                  => Left(s"$path: expected a number but found ${other.typeName}")

  // --- field helpers ----------------------------------------------------------

  /** A field's value, treating an explicit `null` as absence. */
  private def present(obj: Json.Obj, key: String): Option[Json] =
    obj.get(key).filter(_ != Json.Null)

  private def field(obj: Json.Obj, path: String, key: String): Either[String, Json] =
    present(obj, key).toRight(s"$path.$key: missing field")

  /** A field's value verbatim, where an explicit `null` is a value and not
    * absence. The opaque payloads — an artifact and an artifact schema — are read
    * this way, since JSON `null` is a perfectly good artifact and folding it into
    * "missing" would stop it round-tripping. */
  private def raw(obj: Json.Obj, path: String, key: String): Either[String, Json] =
    obj.get(key).toRight(s"$path.$key: missing field")

  private def str(obj: Json.Obj, path: String, key: String): Either[String, String] =
    field(obj, path, key).flatMap:
      case Json.Str(s) => Right(s)
      case other       => Left(s"$path.$key: expected a string but found ${other.typeName}")

  private def int(obj: Json.Obj, path: String, key: String): Either[String, Int] =
    field(obj, path, key).flatMap:
      case Json.Num(n) if n.isValidInt => Right(n.toInt)
      case Json.Num(n)                 => Left(s"$path.$key: expected a whole number but found $n")
      case other                       => Left(s"$path.$key: expected a number but found ${other.typeName}")

  private def bool(obj: Json.Obj, path: String, key: String): Either[String, Boolean] =
    field(obj, path, key).flatMap:
      case Json.Bool(b) => Right(b)
      case other        => Left(s"$path.$key: expected a boolean but found ${other.typeName}")

  private def strList(obj: Json.Obj, path: String, key: String): Either[String, List[String]] =
    field(obj, path, key).flatMap:
      case Json.Arr(elems) =>
        traverse(elems.zipWithIndex):
          case (Json.Str(s), _) => Right(s)
          case (other, index)   => Left(s"$path.$key[$index]: expected a string but found ${other.typeName}")
      case other => Left(s"$path.$key: expected an array but found ${other.typeName}")

  private def optStr(obj: Json.Obj, path: String, key: String): Either[String, Option[String]] =
    present(obj, key) match
      case None                 => Right(None)
      case Some(Json.Str(s))    => Right(Some(s))
      case Some(other)          => Left(s"$path.$key: expected a string but found ${other.typeName}")

  private def optInt(obj: Json.Obj, path: String, key: String): Either[String, Option[Int]] =
    present(obj, key) match
      case None                              => Right(None)
      case Some(Json.Num(n)) if n.isValidInt => Right(Some(n.toInt))
      case Some(Json.Num(n))                 => Left(s"$path.$key: expected a whole number but found $n")
      case Some(other)                       => Left(s"$path.$key: expected a number but found ${other.typeName}")

  private def traverse[A, B](xs: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    xs.foldRight[Either[String, List[B]]](Right(Nil)): (x, acc) =>
      for h <- f(x); t <- acc yield h :: t
