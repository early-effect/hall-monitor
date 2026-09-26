package hallmonitor.forward

import hallmonitor.domain.*
import heddle.client.Client
import heddle.http.header.{HeaderName, Headers}
import heddle.http.{Body, MediaType, Method, Request, Response, Url}
import zio.json.*
import zio.json.ast.Json
import zio.IO

object Forward:
  def send(client: Client, tokens: TokenCache, backend: Backend, payload: Json.Obj): IO[RouteError, Response] =
    tokens.bearer(backend).flatMap { token =>
      val stream              = payload.get("stream").contains(Json.Bool(true))
      val (url, body, accept) = prepared(backend, payload)
      val headers             = Headers.empty
        .add(HeaderName.Authorization, s"Bearer $token")
        .add(HeaderName.Accept, accept)
        .add(HeaderName.ContentType, "application/json")
      val request = Request(Method.POST, Url.parse(url), extra(backend, headers), Body.json(body.toJson))
      client
        .batched(request)
        .mapError(_ => RouteError.Upstream(502, "upstream unreachable"))
        .map(response => translate(backend, response, stream))
    }

  private def prepared(backend: Backend, payload: Json.Obj): (String, Json.Obj, String) =
    val base = backend.baseUrl.stripSuffix("/")
    backend.wire match
      case _: UpstreamWire.Chat =>
        val body = Shape.chat(rewriteModel(payload, backend.upstreamModel), chatCompat(backend))
        (endpoint(base, backend.kind, "chat/completions"), body, accept(body))
      case UpstreamWire.Responses =>
        (
          endpoint(base, ModelKind.Conversational, "responses"),
          Shape.responses(payload, backend.upstreamModel),
          "application/json",
        )
      case UpstreamWire.Messages(_) =>
        (
          endpoint(base, ModelKind.Conversational, "messages"),
          Shape.messages(payload, backend.upstreamModel),
          "application/json",
        )
    end match
  end prepared

  private def chatCompat(backend: Backend): ChatCompat =
    backend.wire match
      case UpstreamWire.Chat(compat) => compat
      case _                         => ChatCompat()

  private def extra(backend: Backend, headers: Headers): Headers =
    val proxied =
      if backend.auth.isInstanceOf[BackendAuth.Grok] && backend.wire.isInstanceOf[UpstreamWire.Chat] then
        headers
          .add(HeaderName("X-XAI-Token-Auth"), "xai-grok-cli")
          .add(HeaderName("x-grok-model-override"), backend.upstreamModel)
      else headers
    backend.wire match
      case UpstreamWire.Messages(extra) =>
        extra.foldLeft(proxied) { (acc, pair) => acc.add(HeaderName(pair._1), pair._2) }
      case _ => proxied
  end extra

  private def translate(backend: Backend, response: Response, stream: Boolean): Response =
    if response.status.code >= 400 then response
    else
      backend.wire match
        case _: UpstreamWire.Chat   => response
        case UpstreamWire.Responses =>
          render(backend.upstreamModel, response, stream, Shape.fromResponses)
        case UpstreamWire.Messages(_) =>
          render(backend.upstreamModel, response, stream, Shape.fromMessages)

  private def render(
      model: String,
      response: Response,
      stream: Boolean,
      decode: (String, Json.Obj) => Json.Obj,
  ): Response =
    response.body.asString.fromJson[Json.Obj] match
      case Left(_)     => response
      case Right(body) =>
        val completion = decode(model, body)
        if stream then
          Response(response.status).withBody(Body.text(Shape.asEventStream(completion), MediaType.EventStream))
        else Response(response.status).withBody(Body.json(completion.toJson))

  private def rewriteModel(payload: Json.Obj, model: String): Json.Obj =
    Json.Obj(payload.fields.filterNot(_._1 == "model") :+ ("model" -> Json.Str(model)))

  private def endpoint(base: String, kind: ModelKind, conversational: String): String =
    kind match
      case ModelKind.Conversational => s"$base/$conversational"
      case ModelKind.Decision       => s"$base/v1/systemone"

  private def accept(payload: Json.Obj): String =
    payload.get("stream") match
      case Some(Json.Bool(true)) => "text/event-stream"
      case _                     => "application/json"
end Forward
