package hallmonitor.http

import hallmonitor.classify.Classify
import hallmonitor.config.LoadError
import hallmonitor.domain.*
import hallmonitor.forward.Forward
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
                    .foldZIO(
                      failure => complete(wire, face, failure, None, loaded, start, id, truncated = false, None),
                      reading =>
                        Resolve(
                          loaded.policy,
                          face,
                          reading.answers,
                          incoming.requested,
                          reading.truncated,
                        ) match
                          case Left(failure) =>
                            complete(wire, face, failure, Some(reading), loaded, start, id, reading.truncated, None)
                          case Right(backend) =>
                            Forward
                              .send(gate.client, gate.tokens, backend, incoming.body)
                              .foldZIO(
                                failure =>
                                  complete(
                                    wire,
                                    face,
                                    failure,
                                    Some(reading),
                                    loaded,
                                    start,
                                    id,
                                    reading.truncated,
                                    Some(backend),
                                  ),
                                response => complete(face, response, reading, loaded, start, id, backend),
                              ),
                    )
                },
        )
    }

  private def complete(
      wire: Wire,
      face: ModelKind,
      failure: RouteError,
      reading: Option[hallmonitor.classify.Reading],
      loaded: Loaded,
      start: Long,
      id: String,
      truncated: Boolean,
      backend: Option[Backend],
  ): UIO[Response] =
    val response = annotate(error(wire, failure), loaded, truncated, backend, asked = reading.exists(_.asked))
    log(id, face, loaded, reading, backend, response.status.code, start, truncated).as(response)
  end complete

  private def complete(
      face: ModelKind,
      response: Response,
      reading: hallmonitor.classify.Reading,
      loaded: Loaded,
      start: Long,
      id: String,
      backend: Backend,
  ): UIO[Response] =
    val annotated = annotate(response, loaded, reading.truncated, Some(backend), asked = reading.asked)
    log(id, face, loaded, Some(reading), Some(backend), annotated.status.code, start, reading.truncated).as(annotated)
  end complete

  private def log(
      id: String,
      face: ModelKind,
      loaded: Loaded,
      reading: Option[hallmonitor.classify.Reading],
      backend: Option[Backend],
      status: Int,
      start: Long,
      truncated: Boolean,
  ): UIO[Unit] =
    Clock.nanoTime.flatMap { end =>
      val host =
        loaded.classifier
          .filter(_ => reading.exists(_.asked))
          .map(value => Classify.authority(value.baseUrl))
          .getOrElse("-")
      val answers = reading.map(value => Classify.summary(value.answers)).filter(_.nonEmpty).getOrElse("-")
      val chosen  = backend.map(_.id.value).getOrElse("-")
      val ms      = (end - start) / 1000000L
      val name    = face match
        case ModelKind.Conversational => "conversational"
        case ModelKind.Decision       => "decision"
      ZIO.logInfo(
        s"$id face=$name classifier=$host $answers backend=$chosen upstream=$status elapsedMs=$ms truncated=$truncated"
      )
    }

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
