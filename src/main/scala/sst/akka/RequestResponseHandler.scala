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