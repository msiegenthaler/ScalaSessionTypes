package sst.akka

import akka.actor._
import shapeless._
import shapeless.syntax.typeable._
import sst._

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
}