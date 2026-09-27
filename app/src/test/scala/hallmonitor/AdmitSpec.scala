package hallmonitor

import hallmonitor.admit.*
import hallmonitor.domain.*
import heddle.client.Client
import heddle.http.{Request, Response, Status}
import heddle.http.header.HeaderName
import hexis.{Answer, Probability}
import zio.test.*
import zio.{durationInt, Chunk, Promise, Ref, Task, ZIO}

object AdmitSpec extends ZIOSpecDefault:
  def spec = suite("Admit")(
    test("an unreachable candidate falls through and the permit is released") {
      val local = backend("local")
      val fast  = backend("fast")
      for
        pool     <- makePool()
        seen     <- Ref.make(List.empty[String])
        result   <- Admit.walk(open(List(local, fast)), pool, Map.empty, 15, respond(seen, fail = Set("local")))
        held     <- pool.inFlight(local.id)
        heldFast <- pool.inFlight(fast.id)
        called   <- seen.get
      yield assertTrue(
        result.served.map(_.id.value).contains("fast"),
        result.failure.isEmpty,
        held == 0,
        heldFast == 0,
        called == List("local", "fast"),
        result.attempts.collect { case Attempt.Called(id, _, _, Some(Fall.Unreachable)) => id.value } == List("local"),
      )
      end for
    },
    test("a saturated backend is not called") {
      val local = backend("local", max = Some(1))
      for
        pool   <- makePool()
        _      <- pool.claim(local.id, Some(1))
        seen   <- Ref.make(0)
        result <- Admit.walk(open(List(local)), pool, Map.empty, 15, _ => seen.update(_ + 1) *> ZIO.succeed(ok))
        calls  <- seen.get
      yield assertTrue(
        calls == 0,
        result.served.isEmpty,
        result.attempts == List(Attempt.Skipped(local.id, Skip.Saturated(1, 1))),
      )
      end for
    },
    test("a quota at the stop line is not called and a failed probe leaves the backend eligible") {
      val name  = QuotaId("grok-build")
      val quota = Quota(name, QuotaKind.GrokWeekly, "~/.grok", 0.9)
      val model = backend("grok", quota = Some(name))
      for
        blocked <- makePool()
        _       <- blocked.putReading(name, QuotaReading(Some(0.96), 0.9, Some("2026-10-01T00:00:00Z"), 0L))
        seen    <- Ref.make(0)
        over    <- Admit.walk(
          open(List(model)),
          blocked,
          Map(name -> quota),
          15,
          _ => seen.update(_ + 1) *> ZIO.succeed(ok),
        )
        calls    <- seen.get
        openPool <- makePool(_ => ZIO.fail(RouteError.Upstream(502, "unread")))
        tried    <- Ref.make(0)
        missed   <- Admit.walk(
          open(List(model)),
          openPool,
          Map(name -> quota),
          15,
          _ => tried.update(_ + 1) *> ZIO.succeed(ok),
        )
        after <- tried.get
      yield assertTrue(
        calls == 0,
        over.attempts.head == Attempt.Skipped(
          model.id,
          Skip.OverQuota("grok-build", 0.96, 0.9, Some("2026-10-01T00:00:00Z")),
        ),
        after == 1,
        missed.served.map(_.id.value).contains("grok"),
      )
      end for
    },
    test("backends that share a grok quota leave together") {
      val name  = QuotaId("grok-build")
      val quota = Quota(name, QuotaKind.GrokWeekly, "~/.grok", 0.9)
      val a     = backend("grok-4.7", quota = Some(name))
      val b     = backend("grok-4.6", quota = Some(name))
      for
        pool   <- makePool()
        _      <- pool.putReading(name, QuotaReading(Some(0.91), 0.9, None, 0L))
        seen   <- Ref.make(List.empty[String])
        result <- Admit.walk(open(List(a, b)), pool, Map(name -> quota), 15, respond(seen, fail = Set.empty))
        called <- seen.get
      yield assertTrue(called.isEmpty, result.served.isEmpty, result.attempts.length == 2)
    },
    test("a lock keeps a removed model out of the walk") {
      val local = backend("local-strong", "http://127.0.0.1:9")
      val fast  = backend("public-fast", "https://api.example")
      val route = Resolve(
        Policy(
          List(local, fast),
          List(Criterion.Noul(CriterionId("pii"), "pii", "yes", "no", ModelKind.both)),
          List(
            Rule.Constraint(
              RuleId("pii-lock"),
              List(Predicate.NoulYesAtLeast(CriterionId("pii"), Probability.unsafely(0.4))),
              Set(local.id),
            )
          ),
          List(fast.id),
          1000,
        ),
        ModelKind.Conversational,
        Map(CriterionId("pii") -> Answer.Noul(Probability.unsafely(0.95))),
        None,
        truncated = false,
      )
      for
        pool   <- makePool()
        seen   <- Ref.make(List.empty[String])
        result <- Admit.walk(route, pool, Map.empty, 15, respond(seen, fail = Set("local-strong")))
        called <- seen.get
      yield assertTrue(
        route.order.map(_.id.value) == List("local-strong"),
        called == List("local-strong"),
        result.served.isEmpty,
        result.failure.exists(_.isInstanceOf[RouteError.NoneAvailable]),
      )
      end for
    },
    test("a 400 is returned and a 429 falls through onto a cooldown") {
      val first  = backend("first")
      val second = backend("second")
      for
        fresh <- makePool()
        bad   <- Admit.walk(
          open(List(first, second)),
          fresh,
          Map.empty,
          15,
          backend =>
            if backend.id == first.id then ZIO.succeed(Response(Status.BadRequest))
            else ZIO.fail(RouteError.Upstream(500, "should not be called")),
        )
        cooled <- makePool()
        seen   <- Ref.make(List.empty[String])
        next   <- Admit.walk(
          open(List(first, second)),
          cooled,
          Map.empty,
          15,
          backend =>
            seen.update(_ :+ backend.id.value) *> (
              if backend.id == first.id then
                ZIO.succeed(Response(Status.TooManyRequests).withHeader(HeaderName.RetryAfter, "5"))
              else ZIO.succeed(ok)
            ),
        )
        until  <- cooled.coolingUntil(first.id, 0L)
        called <- seen.get
      yield assertTrue(
        bad.response.map(_.status).contains(Status.BadRequest),
        bad.served.map(_.id).contains(first.id),
        next.served.map(_.id).contains(second.id),
        called == List("first", "second"),
        until.contains(5_000L),
      )
      end for
    },
    test("without a liveness check a down backend is retried after the window") {
      val remote = backend("remote", "https://api.example")
      for
        pool <- makePool()
        seen <- Ref.make(0)
        _    <- Admit.walk(
          open(List(remote)),
          pool,
          Map.empty,
          15,
          _ => seen.update(_ + 1) *> ZIO.fail(RouteError.Unreachable("upstream unreachable")),
        )
        second <- Admit.walk(open(List(remote)), pool, Map.empty, 15, _ => seen.update(_ + 1) *> ZIO.succeed(ok))
        mid    <- seen.get
        _      <- TestClock.adjust(15.seconds)
        third  <- Admit.walk(open(List(remote)), pool, Map.empty, 15, _ => seen.update(_ + 1) *> ZIO.succeed(ok))
        after  <- seen.get
      yield assertTrue(
        mid == 1,
        second.attempts.head.isInstanceOf[Attempt.Skipped],
        third.served.map(_.id).contains(remote.id),
        after == 2,
      )
      end for
    },
    test("interrupting an attempt releases the permit") {
      val model = backend("local")
      for
        pool    <- makePool()
        started <- Promise.make[Nothing, Unit]
        hold    <- Promise.make[Nothing, Unit]
        fiber   <- Admit
          .walk(
            open(List(model)),
            pool,
            Map.empty,
            15,
            _ => started.succeed(()) *> hold.await *> ZIO.succeed(ok),
          )
          .fork
        _    <- started.await
        _    <- fiber.interrupt
        held <- pool.inFlight(model.id)
      yield assertTrue(held == 0)
      end for
    },
    test("an up check puts a down backend back and does not clear quota or a cooldown") {
      val model = backend("local", "http://127.0.0.1:9", liveness = Some(Liveness(1, 1, None, enabled = true)))
      val name  = QuotaId("grok-build")
      for
        pool   <- makePool()
        _      <- pool.noteDown(model.id, 0L, None)
        _      <- pool.putReading(name, QuotaReading(Some(0.99), 0.9, None, 0L))
        _      <- pool.noteUp(model.id, 1L)
        down   <- pool.downSince(model.id, 1L)
        seen   <- Ref.make(0)
        result <- Admit.walk(
          open(List(model)),
          pool,
          Map.empty,
          15,
          _ => seen.update(_ + 1) *> ZIO.succeed(ok),
        )
        calls <- seen.get
        _     <- pool.armCooldown(model.id, 30_000L)
        cool  <- pool.coolingUntil(model.id, 1L)
        read  <- pool.reading(name)
        again <- Admit.walk(
          open(List(model)),
          pool,
          Map.empty,
          15,
          _ => ZIO.fail(RouteError.Upstream(500, "cooldown should skip")),
        )
      yield assertTrue(
        down.isEmpty,
        calls == 1,
        result.served.isDefined,
        cool.contains(30_000L),
        read.flatMap(_.used).contains(0.99),
        again.attempts == List(Attempt.Skipped(model.id, Skip.CoolingDown(30_000L))),
      )
      end for
    },
    test("a repeated down check logs one transition") {
      val model = backend("local", "http://127.0.0.1:9", liveness = Some(Liveness(1, 1, None, enabled = true)))
      for
        pool    <- makePool()
        current <- Ref.make(loaded(List(model)))
        due     <- Ref.make(Map.empty[BackendId, Long])
        _       <- Checks.tick(current, pool, failing, due)
        first   <- pool.transitionsSoFar
        _       <- TestClock.adjust(1.second)
        _       <- Checks.tick(current, pool, failing, due)
        second  <- pool.transitionsSoFar
      yield assertTrue(first == Chunk("local down"), second == Chunk("local down"))
      end for
    },
    test("loopback is checked, a remote without a table is not, and a 404 without a path is up") {
      val local  = backend("local", "http://127.0.0.1:9")
      val remote = backend("remote", "https://api.example")
      for
        pool    <- makePool()
        current <- Ref.make(loaded(List(local, remote)))
        due     <- Ref.make(Map.empty[BackendId, Long])
        seen    <- Ref.make(List.empty[String])
        _       <- Checks.tick(current, pool, recording(seen, Status.NotFound), due)
        urls    <- seen.get
        snap    <- pool.snapshot(List(local, remote))
      yield assertTrue(
        urls.exists(_.contains("127.0.0.1")),
        !urls.exists(_.contains("api.example")),
        snap.find(_.id == local.id).map(_.presence).contains("up"),
        snap.find(_.id == remote.id).map(_.presence).contains("unchecked"),
      )
      end for
    },
    test("a configured path that is not 2xx marks the backend down") {
      val local =
        backend("local", "http://127.0.0.1:9", liveness = Some(Liveness(5, 1, Some("/health"), enabled = true)))
      for
        pool    <- makePool()
        current <- Ref.make(loaded(List(local)))
        due     <- Ref.make(Map.empty[BackendId, Long])
        seen    <- Ref.make(List.empty[String])
        _       <- Checks.tick(current, pool, recording(seen, Status.NotFound), due)
        snap    <- pool.snapshot(List(local))
        urls    <- seen.get
      yield assertTrue(
        urls.exists(_.contains("/health")),
        snap.map(_.presence) == List("down"),
      )
      end for
    },
    test("closing the liveness scope stops the loop") {
      val local = backend("local", "http://127.0.0.1:9", liveness = Some(Liveness(1, 1, None, enabled = true)))
      for
        pool    <- makePool()
        current <- Ref.make(loaded(List(local)))
        seen    <- Ref.make(0)
        client = new Client:
          def batched(req: Request): Task[Response] =
            seen.update(_ + 1).as(Response(Status.Ok))
        _     <- ZIO.scoped(Checks.supervise(current, pool, client).forkScoped *> TestClock.adjust(1.second))
        mid   <- seen.get
        _     <- TestClock.adjust(5.seconds)
        after <- seen.get
      yield assertTrue(mid >= 1, after == mid)
      end for
    },
    test("an explicit disabled table turns off the loopback default") {
      val local = backend(
        "local",
        "http://127.0.0.1:9",
        liveness = Some(Liveness(5, 1, None, enabled = false)),
      )
      assertTrue(Checks.effective(local).isEmpty, Checks.effective(backend("remote", "https://api.example")).isEmpty)
    },
    test("the credits payload is a fraction and anything else is unknown") {
      val weekly = """{"config":{"creditUsagePercent":90,"currentPeriod":{"end":"2026-10-01T00:00:00Z"}}}"""
      val other  = """{"config":{"monthlyLimit":{"val":10}}}"""
      import zio.json.*
      import zio.json.ast.Json
      assertTrue(
        GrokQuota.parse(weekly.fromJson[Json].toOption.get) == Some(QuotaWindow(0.9, Some("2026-10-01T00:00:00Z"))),
        GrokQuota.parse(other.fromJson[Json].toOption.get).isEmpty,
      )
    },
  )

  private def makePool(
      probe: Quota => zio.IO[RouteError, QuotaWindow] = _ => ZIO.fail(RouteError.Upstream(502, "no probe"))
  ) =
    Pool.make(probe)

  private def open(backends: List[Backend]): RoutePlan =
    RoutePlan(backends, Trace(None, Map.empty, Nil, backends.map(_.id), truncated = false, unclassified = false), None)

  private def respond(seen: Ref[List[String]], fail: Set[String]): Backend => zio.IO[RouteError, Response] =
    backend =>
      seen.update(_ :+ backend.id.value) *> (
        if fail.contains(backend.id.value) then ZIO.fail(RouteError.Unreachable("upstream unreachable"))
        else ZIO.succeed(ok)
      )

  private val ok: Response = Response.json("""{"ok":true}""")

  private val failing: Client =
    new Client:
      def batched(req: Request): Task[Response] =
        ZIO.fail(new java.io.IOException("connection refused"))

  private def recording(seen: Ref[List[String]], status: Status): Client =
    new Client:
      def batched(req: Request): Task[Response] =
        seen.update(_ :+ req.url.render).as(Response(status))

  private def loaded(backends: List[Backend]): Loaded =
    Loaded(
      Policy(backends, Nil, Nil, Nil, 1000),
      Secret("k"),
      None,
      Listen("127.0.0.1", 8080),
      30,
    )

  private def backend(
      id: String,
      url: String = "https://example.test",
      max: Option[Int] = None,
      liveness: Option[Liveness] = None,
      quota: Option[QuotaId] = None,
  ): Backend =
    Backend(
      BackendId(id),
      ModelKind.Conversational,
      url,
      id,
      BackendAuth.Key(Secret("k")),
      Nil,
      maxInFlight = max,
      liveness = liveness,
      quota = quota,
    )
end AdmitSpec
