package hallmonitor.domain

enum RouteError:
  case Unauthorized
  case Malformed(detail: String)
  case UnknownModel(name: String)
  case ModelNotAllowed(name: String, allowed: List[BackendId])
  case NoEligibleBackend
  case Classifier(detail: String)
  case Upstream(status: Int, body: String)

  def message: String =
    this match
      case Unauthorized                   => "missing or invalid bearer token"
      case Malformed(detail)              => detail
      case UnknownModel(name)             => s"unknown model $name"
      case ModelNotAllowed(name, allowed) =>
        val names = allowed.map(_.value).mkString(", ")
        s"model $name is not allowed for this request (allowed: $names)"
      case NoEligibleBackend  => "no backend is allowed for this request"
      case Classifier(detail) => detail
      case Upstream(_, body)  => body
end RouteError
