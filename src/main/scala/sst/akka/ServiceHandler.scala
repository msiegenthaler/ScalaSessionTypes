package sst.akka

import _root_.akka.actor._
import sst._
import shapeless._
import shapeless.syntax.typeable._

/** Handler for simple services (one request, one response). */
trait SingleServiceHandler[A <: Action] {
  type Request
  type Response
  protected[akka] implicit def requestTypeable: Typeable[Request]

  def handle(f: Request => Response)(implicit context: ActorContext): PartialFunction[Any, Unit] = {
    case req if req.cast[Request].isDefined =>
      val typedRequest = req.cast[Request].get
      context.sender ! f(typedRequest)
  }
}

object Service {
  def apply[A <: Action](implicit h: SingleServiceHandler[A]): Aux[A, h.Request, h.Response] = h

  type Aux[A <: Action, Req, Resp] = SingleServiceHandler[A] {type Request = Req; type Response = Resp}
  implicit def singleRequestResponse[Req: Typeable, Resp]: Aux[Then[?[Req], ![Resp]], Req, Resp] = new SingleServiceHandler[Then[?[Req], ![Resp]]] {
    type Request = Req
    type Response = Resp
    val requestTypeable = implicitly[Typeable[Req]]
  }
  implicit def repeatedSingleRequestResponse[A <: Action](implicit h: SingleServiceHandler[A]): Aux[Repeat[A], h.Request, h.Response] = new SingleServiceHandler[Repeat[A]] {
    type Request = h.Request
    type Response = h.Response
    val requestTypeable = h.requestTypeable
  }
}
