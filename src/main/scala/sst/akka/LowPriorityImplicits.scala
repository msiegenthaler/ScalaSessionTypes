package sst.akka

import scala.language.implicitConversions
import sst._

class LowPriorityImplicits {
  /** Ask for coproduct-response request/response. */
  implicit def requestCoproductResponseHandlerFactory[A <: Action](a: A)(implicit rr: RequestResponse[A]) =
    new RequestCoproductResponseHandlerFactory[A, rr.Request, rr.Response](a, rr)
}
