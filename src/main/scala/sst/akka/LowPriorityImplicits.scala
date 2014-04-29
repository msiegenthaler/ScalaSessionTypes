package sst.akka

import scala.language.implicitConversions
import sst._

class LowPriorityImplicits {
  implicit def requestCoproductResponseHandler[A <: Action](a: A)(implicit rr: RequestResponse[A]) =
    new RequestCoproductResponseHandler[A, rr.Request, rr.Response](a, rr)

  implicit def coproductNotificationSubscriptionHandler[A <: Action](a: A)(implicit s: Subscription[A]) =
    new CoproductNotificationSubscriptionHandler[A, s.Setup, s.Message](a, s)
}
