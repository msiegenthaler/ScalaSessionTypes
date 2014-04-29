package sst.akka

import scala.language.implicitConversions
import sst._

class LowPriorityImplicits {
  /** Ask for coproduct-response request/response. */
  implicit def requestCoproductResponseHandler[A <: Action](a: A)(implicit rr: RequestResponse[A]) =
    new RequestCoproductResponseHandler[A, rr.Request, rr.Response](a, rr)
}
