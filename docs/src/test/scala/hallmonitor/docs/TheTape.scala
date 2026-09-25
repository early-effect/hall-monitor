package hallmonitor.docs

import specular.*
import specular.ziotest.DocSpecSuite

object TheTape extends DocSpecSuite:
  def doc = page("The tape")(
    md"""
`stream: true` comes back as a real event stream. The song is recorded all the
way through, then played from the top. Time to first token is the length of
the song. The harness still hears an event stream. It just hears the intro
once the band has already left the studio.

Heddle's client brings the upstream body home in one piece. Hall Monitor hands
that piece over with the upstream status and content type. A client that
already knows OpenAI frames still knows these.

The bytes stay the upstream's bytes. Long answers want a patient idle timeout:
`upstreamIdleSeconds`, five minutes in the sample. The classifier keeps a
shorter clock. Picking who speaks should finish before the speech does.
"""
  )
end TheTape
