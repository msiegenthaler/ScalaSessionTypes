package sst.akka

import scala.concurrent.ExecutionContext
import akka.actor.ActorRef
import akka.util.Timeout
import sst.{akka => _, _}
import shapeless._

final class RequestSingleResponseHandler[A <: Action, Request, Response](action: Action, rsr: RequestSingleResponse[A]) {
  def apply(actor: ActorRef): AskActorRef[A, Request, Response] = {
    new AskActorRef[A, Request, Response](action, actor) {
      override def ask(in: Request)(implicit timeout: Timeout, ec: ExecutionContext) =
        akka.pattern.ask(actor, in).map { response =>
          rsr.parse(response).
            getOrElse(throw new MatchError(s"Unexpected response (wrong type): $response to $action")).
            asInstanceOf[Response] //this is safe
        }
    }
  }
}

final class RequestCoproductResponseHandler[A <: Action, Request, Response <: Coproduct](action: Action, rr: RequestResponse[A]) {
  def apply(actor: ActorRef): AskActorRef[A, Request, Response] = {
    new AskActorRef[A, Request, Response](action, actor) {
      override def ask(in: Request)(implicit timeout: Timeout, ec: ExecutionContext) =
        akka.pattern.ask(actor, in).map { response =>
          rr.parse(response).
            getOrElse(throw new MatchError(s"Unexpected response (wrong type): $response to $action")).
            asInstanceOf[Response] //this is safe
        }
    }
  }
}