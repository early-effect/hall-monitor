package hallmonitor.http

import hallmonitor.admit.{Admit, Pool}
import hallmonitor.classify.Classify
import hallmonitor.config.LoadError
import hallmonitor.domain.*
import hallmonitor.forward.Forward
import hallmonitor.journal.{CallRecord, Journal, Timings}
import heddle.*
import heddle.client.Client
import zio.json.*
import zio.json.ast.Json
import zio.*

final case class Gate(
    current: Ref[Loaded],
    transport: hexis.Transport,
    client: Client,
    tokens: hallmonitor.forward.TokenCache,
    reload: IO[LoadError, Loaded],
    journal: Journal,
    pool: Pool,
)

object Api:
  def routes(gate: Gate): Routes[Any, Nothing] =
    Routes(
      Method.GET / "health"        -> Handler.fromFunctionZIO(_ => ZIO.succeed(Response.text("ok"))),
      Method.GET / "v1" / "models" -> Handler.fromFunctionZIO(models(gate, ModelKind.Conversational, Wire.OpenAi)),
      Method.GET / "jev" / "v1" / "models" -> Handler.fromFunctionZIO(models(gate, ModelKind.Decision, Wire.Jev)),
      Method.POST / "v1" / "chat" / "completions" ->
        Handler.fromFunctionZIO(call(gate, ModelKind.Conversational, Wire.OpenAi)),
      Method.POST / "jev" / "v1" / "systemone" ->
        Handler.fromFunctionZIO(call(gate, ModelKind.Decision, Wire.Jev)),
      Method.POST / "admin" / "reload" -> Handler.fromFunctionZIO(reload(gate)),
      Method.GET / "admin" / "calls"   -> Handler.fromFunctionZIO(calls(gate)),
      Method.GET / "admin" / "pool"    -> Handler.fromFunctionZIO(pool(gate)),
    ) @@ Middleware.requestId()

  private def models(gate: Gate, face: ModelKind, wire: Wire)(request: Request): UIO[Response] =
    gate.current.get.map { loaded =>
      if !authorized(request, loaded.apiKey) then error(wire, RouteError.Unauthorized)
      else
        val body = face match
          case ModelKind.Conversational => openAiModels(loaded.policy)
          case ModelKind.Decision       => jevModels(loaded.policy)
        Response.json(body.toJson)
    }

  private def reload(gate: Gate)(request: Request): UIO[Response] =
    gate.current.get.flatMap { loaded =>
      if !authorized(request, loaded.apiKey) then ZIO.succeed(error(Wire.OpenAi, RouteError.Unauthorized))
      else
        gate.reload.foldZIO(
          failure => ZIO.succeed(json(Status.UnprocessableContent, errors(failure.messages))),
          next => gate.current.set(next) *> ZIO.succeed(Response.json("""{"reloaded":true}""")),
        )
    }

  private def calls(gate: Gate)(request: Request): UIO[Response] =
    gate.current.get.flatMap { loaded =>
      if !authorized(request, loaded.apiKey) then ZIO.succeed(error(Wire.OpenAi, RouteError.Unauthorized))
      else
        gate.journal.recent.map { records =>
          val body = Json.Obj("calls" -> Json.Arr(records.map(Journal.view)))
          Response.json(body.toJson)
        }
    }

  private def pool(gate: Gate)(request: Request): UIO[Response] =
    gate.current.get.flatMap { loaded =>
      if !authorized(request, loaded.apiKey) then ZIO.succeed(error(Wire.OpenAi, RouteError.Unauthorized))
      else
        gate.pool.snapshot(loaded.policy.backends).map { entries =>
          val body = Json.Obj(
            "backends" -> Json.Arr(
              Chunk.fromIterable(
                entries.map { entry =>
                  Json.Obj(
                    "id"        -> Json.Str(entry.id.value),
                    "presence"  -> Json.Str(entry.presence),
                    "sinceMs"   -> Json.Num(entry.sinceMs),
                    "checkedMs" -> Json.Num(entry.checkedMs),
                  )
                }
              )
            )
          )
          Response.json(body.toJson)
        }
    }

  private def call(gate: Gate, face: ModelKind, wire: Wire)(request: Request): UIO[Response] =
    gate.current.get.flatMap { loaded =>
      if !authorized(request, loaded.apiKey) then ZIO.succeed(error(wire, RouteError.Unauthorized))
      else
        val id = request.header("X-Request-Id").getOrElse("-")
        request.body.utf8.foldZIO(
          _ => ZIO.succeed(error(wire, RouteError.Malformed("unreadable body"))),
          raw =>
            Ingress.parse(face, raw) match
              case Left(failure)   => ZIO.succeed(error(wire, failure))
              case Right(incoming) =>
                Clock.nanoTime.flatMap { start =>
                  Classify
                    .run(
                      loaded.policy,
                      loaded.classifier,
                      face,
                      hallmonitor.classify.Subject(incoming.requested, incoming.text, incoming.toolNames),
                      gate.transport,
                    )
                    .either
                    .flatMap { classified =>
                      Clock.nanoTime.flatMap { after =>
                        val classifyFor = Duration.fromNanos(after - start)
                        classified match
                          case Left(failure) if loaded.classifier.isEmpty =>
                            val plan = Resolve(loaded.policy, face, Map.empty, incoming.requested, truncated = false)
                            finish(
                              gate,
                              face,
                              loaded,
                              id,
                              start,
                              classifyFor,
                              None,
                              plan.copy(rejected = Some(failure), order = Nil),
                              Nil,
                              None,
                              Some(failure),
                              asked = false,
                              error(wire, failure),
                            )
                          case Left(RouteError.Classifier(detail)) =>
                            val plan = Resolve(
                              loaded.policy,
                              face,
                              Map.empty,
                              incoming.requested,
                              truncated = false,
                              unclassified = true,
                            )
                            admit(
                              gate,
                              wire,
                              face,
                              loaded,
                              id,
                              start,
                              classifyFor,
                              None,
                              plan,
                              Some(detail),
                              incoming.body,
                            )
                          case Left(failure) =>
                            val plan = Resolve(loaded.policy, face, Map.empty, incoming.requested, truncated = false)
                            finish(
                              gate,
                              face,
                              loaded,
                              id,
                              start,
                              classifyFor,
                              None,
                              plan.copy(rejected = Some(failure), order = Nil),
                              Nil,
                              None,
                              Some(failure),
                              asked = false,
                              error(wire, failure),
                            )
                          case Right(reading) =>
                            val plan = Resolve(
                              loaded.policy,
                              face,
                              reading.answers,
                              incoming.requested,
                              reading.truncated,
                            )
                            val host =
                              if reading.asked then loaded.classifier.map(value => Classify.authority(value.baseUrl))
                              else None
                            admit(
                              gate,
                              wire,
                              face,
                              loaded,
                              id,
                              start,
                              classifyFor,
                              host,
                              plan,
                              None,
                              incoming.body,
                              reading.asked,
                            )
                        end match
                      }
                    }
                },
        )
    }

  private def admit(
      gate: Gate,
      wire: Wire,
      face: ModelKind,
      loaded: Loaded,
      id: String,
      start: Long,
      classifyFor: Duration,
      classifier: Option[String],
      plan: RoutePlan,
      classifierDetail: Option[String],
      body: Json.Obj,
      asked: Boolean = true,
  ): UIO[Response] =
    plan.rejected match
      case Some(failure) =>
        finish(
          gate,
          face,
          loaded,
          id,
          start,
          classifyFor,
          classifier,
          plan,
          Nil,
          None,
          Some(failure),
          asked,
          error(wire, failure),
        )
      case None =>
        Admit
          .walk(
            plan,
            gate.pool,
            loaded.quotas,
            loaded.downForSeconds,
            backend => Forward.send(gate.client, gate.tokens, backend, body),
          )
          .flatMap { admission =>
            val failure = admission.failure.map {
              case RouteError.NoneAvailable(text) =>
                classifierDetail.fold(RouteError.NoneAvailable(text))(why =>
                  RouteError.NoneAvailable(s"classifier failed ($why); $text")
                )
              case other => other
            }
            val response = admission.response.getOrElse(
              error(wire, failure.getOrElse(RouteError.NoneAvailable("no backend accepted the call")))
            )
            finish(
              gate,
              face,
              loaded,
              id,
              start,
              classifyFor,
              classifier,
              plan,
              admission.attempts,
              admission.served,
              failure,
              asked,
              response,
            )
          }
    end match
  end admit

  private def finish(
      gate: Gate,
      face: ModelKind,
      loaded: Loaded,
      id: String,
      start: Long,
      classifyFor: Duration,
      classifier: Option[String],
      plan: RoutePlan,
      attempts: List[hallmonitor.admit.Attempt],
      served: Option[Backend],
      failure: Option[RouteError],
      asked: Boolean,
      response: Response,
  ): UIO[Response] =
    Clock.nanoTime.flatMap { end =>
      val record = CallRecord(
        id,
        face,
        classifier,
        plan,
        attempts,
        served.map(_.id),
        Timings(classifyFor, Duration.fromNanos(end - start)),
        failure,
      )
      val marked = timings(annotate(response, loaded, plan.trace.truncated, served, asked), record, asked)
      gate.journal.publish(record).as(marked)
    }

  private def timings(response: Response, record: CallRecord, asked: Boolean): Response =
    val total        = response.withHeader("x-hall-monitor-total-ms", millis(record.timings.total))
    val withClassify =
      if asked then total.withHeader("x-hall-monitor-classify-ms", millis(record.timings.classify)) else total
    val upstream = record.attempts.collectFirst {
      case called: hallmonitor.admit.Attempt.Called
          if record.served.contains(called.backend) && called.fellThrough.isEmpty && called.status.isDefined =>
        called.elapsed
    }
    val withUpstream =
      upstream.fold(withClassify)(elapsed => withClassify.withHeader("x-hall-monitor-upstream-ms", millis(elapsed)))
    val first = record.plan.order.headOption.map(_.id)
    (first, record.served) match
      case (Some(from), Some(to)) if from != to =>
        withUpstream.withHeader("x-hall-monitor-fallback-from", from.value)
      case _ => withUpstream
  end timings

  private def millis(duration: Duration): String =
    duration.toMillis.toString

  private def annotate(
      response: Response,
      loaded: Loaded,
      truncated: Boolean,
      backend: Option[Backend],
      asked: Boolean,
  ): Response =
    val withBackend    = backend.fold(response)(value => response.withHeader("x-hall-monitor-backend", value.id.value))
    val withClassifier =
      if asked then
        loaded.classifier.fold(withBackend)(value =>
          withBackend.withHeader("x-hall-monitor-classifier", Classify.authority(value.baseUrl))
        )
      else withBackend
    if asked then withClassifier.withHeader("x-hall-monitor-truncated", truncated.toString) else withClassifier
  end annotate

  private def authorized(request: Request, expected: Secret): Boolean =
    request.headers.get[Authorization] match
      case Some(Authorization(AuthScheme.Bearer, token)) if token.nonEmpty =>
        java.security.MessageDigest.isEqual(
          expected.reveal.getBytes(java.nio.charset.StandardCharsets.UTF_8),
          token.getBytes(java.nio.charset.StandardCharsets.UTF_8),
        )
      case _ => false

  private def openAiModels(policy: Policy): Json =
    val ids = Names.Auto :: policy.backends.filter(_.kind == ModelKind.Conversational).map(_.id.value)
    Json.Obj(
      "object" -> Json.Str("list"),
      "data"   -> Json.Arr(
        Chunk.fromIterable(
          ids.map(id =>
            Json.Obj(
              "id"       -> Json.Str(id),
              "object"   -> Json.Str("model"),
              "owned_by" -> Json.Str("hall-monitor"),
            )
          )
        )
      ),
    )
  end openAiModels

  private def jevModels(policy: Policy): Json =
    Json.Obj(
      "models" -> Json.Arr(
        Chunk.fromIterable(
          policy.backends.filter(_.kind == ModelKind.Decision).map { backend =>
            Json.Obj(
              "name"         -> Json.Str(backend.id.value),
              "description"  -> Json.Str(backend.upstreamModel),
              "release_date" -> Json.Str("n/a"),
            )
          }
        )
      )
    )

  private def error(wire: Wire, failure: RouteError): Response =
    val status = failure match
      case RouteError.Unauthorized          => Status.Unauthorized
      case RouteError.Malformed(_)          => Status.BadRequest
      case RouteError.UnknownModel(_)       => Status.NotFound
      case RouteError.ModelNotAllowed(_, _) => Status.Forbidden
      case RouteError.NoEligibleBackend     => Status.Conflict
      case RouteError.Classifier(_)         => Status.ServiceUnavailable
      case RouteError.Unreachable(_)        => Status.BadGateway
      case RouteError.NoneAvailable(_)      => Status.ServiceUnavailable
      case RouteError.Upstream(code, _)     => Status.fromCode(code)
    json(status, wire.body(failure))
  end error

  private def errors(messages: List[String]): String =
    Json.Obj("errors" -> Json.Arr(Chunk.fromIterable(messages.map(Json.Str(_))))).toJson

  private def json(status: Status, body: String): Response =
    Response(status).withBody(Body.json(body))

  private enum Wire:
    case OpenAi
    case Jev

    def body(failure: RouteError): String =
      val (tpe, code) = failure match
        case RouteError.Unauthorized          => ("invalid_request_error", "unauthorized")
        case RouteError.Malformed(_)          => ("invalid_request_error", "malformed")
        case RouteError.UnknownModel(_)       => ("invalid_request_error", "model_not_found")
        case RouteError.ModelNotAllowed(_, _) => ("invalid_request_error", "model_not_allowed")
        case RouteError.NoEligibleBackend     => ("invalid_request_error", "no_eligible_backend")
        case RouteError.Classifier(_)         => ("server_error", "classifier")
        case RouteError.Unreachable(_)        => ("server_error", "unreachable")
        case RouteError.NoneAvailable(_)      => ("server_error", "none_available")
        case RouteError.Upstream(_, _)        => ("server_error", "upstream")
      this match
        case OpenAi =>
          Json
            .Obj(
              "error" -> Json.Obj(
                "message" -> Json.Str(failure.message),
                "type"    -> Json.Str(tpe),
                "code"    -> Json.Str(code),
              )
            )
            .toJson
        case Jev =>
          Json
            .Obj(
              "detail" -> Json.Obj(
                "error_type" -> Json.Str(code),
                "message"    -> Json.Str(failure.message),
              )
            )
            .toJson
      end match
    end body
  end Wire
end Api
