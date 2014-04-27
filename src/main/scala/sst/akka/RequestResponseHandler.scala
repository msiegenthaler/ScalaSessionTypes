package sst.akka

import scala.concurrent._
import akka.actor.ActorRef
import akka.util.Timeout
import sst.{akka => _, _}
import shapeless.Typeable
import shapeless.syntax.typeable._

class RequestResponseHandler[Request, Response](actor: ActorRef, mapper: Any => Response) {
  def ?(msg: Request)(implicit timeout: Timeout, ec: ExecutionContext) = ask(msg)
  def ask(msg: Request)(implicit timeout: Timeout, ec: ExecutionContext): Future[Response] = {
    akka.pattern.ask(actor, msg).map(mapper)
  }
}

/** Request with only one response variant. */
trait RequestSingleResponse[A <: Action] {
  type Request
  type Response
  def parse(value: Any): Option[Response]
}
object RequestSingleResponse {
  type Aux[A <: Action, Req, Resp] = RequestSingleResponse[A] {
    type Request = Req;
    type Response = Resp
  }

  implicit def simpleRsr[Req, Resp: Typeable]: Aux[![Req] :>: ?[Resp], Req, Resp] = new RequestSingleResponse[![Req] :>: ?[Resp]] {
    type Request = Req
    type Response = Resp
    def parse(value: Any) = value.cast[Resp]
  }
  implicit def repeatedRsr[A <: Action](implicit rsr: RequestSingleResponse[A]): Aux[Repeat[A], rsr.Request, rsr.Response] = new RequestSingleResponse[Repeat[A]] {
    type Request = rsr.Request
    type Response = rsr.Response
    def parse(value: Any) = rsr.parse(value)
  }
}

class RequestSingleResponseHandlerFactory[A <: Action, Request, Response](a: Action, rsr: RequestSingleResponse[A]) {
  def apply(actor: ActorRef): RequestResponseHandler[Request, Response] = {
    new RequestResponseHandler(actor, mapper)
  }
  private def mapper(msg: Any): Response = {
    rsr.parse(msg).
      getOrElse(throw new MatchError(s"Unexpected response (wrong type): $msg to $a")).
      asInstanceOf[Response] //this is safe
  }
}