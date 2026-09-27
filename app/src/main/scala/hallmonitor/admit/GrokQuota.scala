package hallmonitor.admit

import hallmonitor.domain.*
import hallmonitor.forward.GrokSession
import heddle.client.Client
import heddle.http.header.{HeaderName, Headers}
import heddle.http.{Body, Method, Request, Url}
import zio.json.ast.Json
import zio.json.*
import zio.*

object GrokQuota:
  val Endpoint: String = "https://cli-chat-proxy.grok.com/v1/billing?format=credits"

  /** `used` is a fraction of the weekly pool. `None` when the payload is not the credits shape. */
  def parse(json: Json): Option[QuotaWindow] =
    json match
      case obj: Json.Obj =>
        obj.get("config") match
          case Some(config: Json.Obj) =>
            number(config, "creditUsagePercent").filter(percent => percent >= 0 && percent <= 100).map { percent =>
              val resetsAt = config.get("currentPeriod") match
                case Some(period: Json.Obj) =>
                  period.get("end") match
                    case Some(Json.Str(value)) if value.nonEmpty => Some(value)
                    case _                                       => None
                case _ => None
              QuotaWindow(percent / 100.0, resetsAt)
            }
          case _ => None
      case _ => None

  def fetch(quota: Quota, client: Client, discovery: Ref[Option[String]]): IO[RouteError, QuotaWindow] =
    GrokSession.token(quota.home, client, discovery).flatMap { token =>
      val request = Request(
        Method.GET,
        Url.parse(Endpoint),
        Headers.empty
          .add(HeaderName.Authorization, s"Bearer $token")
          .add(HeaderName("X-XAI-Token-Auth"), "xai-grok-cli")
          .add(HeaderName.Accept, "application/json"),
        Body.empty,
      )
      client.batched(request).mapError(_ => RouteError.Upstream(502, "grok quota unreachable")).flatMap { response =>
        if response.status.code != 200 then ZIO.fail(RouteError.Upstream(response.status.code, "grok quota unread"))
        else
          response.body.asString.fromJson[Json] match
            case Right(json) =>
              ZIO.fromOption(parse(json)).orElseFail(RouteError.Upstream(502, "grok quota unrecognized"))
            case Left(_) => ZIO.fail(RouteError.Upstream(502, "grok quota unrecognized"))
      }
    }

  private def number(obj: Json.Obj, name: String): Option[Double] =
    obj.get(name) match
      case Some(Json.Num(value)) => Some(value.doubleValue)
      case _                     => None
end GrokQuota
