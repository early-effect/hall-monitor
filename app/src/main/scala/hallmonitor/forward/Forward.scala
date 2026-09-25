package hallmonitor.forward

import hallmonitor.domain.*
import heddle.client.Client
import heddle.http.header.{HeaderName, Headers}
import heddle.http.{Body, Method, Request, Response, Url}
import zio.json.*
import zio.json.ast.Json
import zio.IO

object Forward:
  def send(client: Client, backend: Backend, payload: Json.Obj): IO[RouteError, Response] =
    val body    = Json.Obj(payload.fields.filterNot(_._1 == "model") :+ ("model" -> Json.Str(backend.upstreamModel)))
    val request = Request(
      Method.POST,
      Url.parse(endpoint(backend)),
      Headers.empty
        .add(HeaderName.Authorization, s"Bearer ${backend.apiKey.reveal}")
        .add(HeaderName.Accept, accept(payload)),
      Body.json(body.toJson),
    )
    client
      .batched(request)
      .mapError(_ => RouteError.Upstream(502, "upstream unreachable"))
  end send

  private def endpoint(backend: Backend): String =
    val base = backend.baseUrl.stripSuffix("/")
    backend.kind match
      case ModelKind.Conversational => s"$base/chat/completions"
      case ModelKind.Decision       => s"$base/v1/systemone"

  private def accept(payload: Json.Obj): String =
    payload.get("stream") match
      case Some(Json.Bool(true)) => "text/event-stream"
      case _                     => "application/json"
end Forward
