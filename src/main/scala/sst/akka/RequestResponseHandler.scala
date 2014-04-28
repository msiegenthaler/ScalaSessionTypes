package sst.akka

import scala.concurrent._
import akka.actor.ActorRef
import akka.util.Timeout
import shapeless._
import shapeless.ops.coproduct.Selector
import sst.{akka => _, _}
import sst.utils._
import sst.utils.CoproductOps.{Contains, Remove}

final class RequestResponseHandler[Request, Response](actor: ActorRef, mapper: Any => Response) {
  def ?(msg: Request)(implicit timeout: Timeout, ec: ExecutionContext) = ask(msg)
  def ask(msg: Request)(implicit timeout: Timeout, ec: ExecutionContext): Future[Response] = {
    akka.pattern.ask(actor, msg).map(mapper)
  }
}

final class RequestSingleResponseHandlerFactory[A <: Action, Request, Response](a: Action, rsr: RequestSingleResponse[A]) {
  def apply(actor: ActorRef): RequestResponseHandler[Request, Response] =
    new RequestResponseHandler(actor, mapper)

  private def mapper(msg: Any): Response = {
    rsr.parse(msg).
      getOrElse(throw new MatchError(s"Unexpected response (wrong type): $msg to $a")).
      asInstanceOf[Response] //this is safe
  }
}


final class RequestCoproductResponseHandler[Request, Response <: Coproduct](actor: ActorRef, parse: Any => Response) {
  def map[A](f: Response => A): RequestResponseHandler[Request, A] =
    new RequestResponseHandler[Request, A](actor, parse.andThen(f))

  def mapCoproduct[A](f: CoproductFold[Response, Response] => Response => A): RequestResponseHandler[Request, A] = {
    val cf = CoproductFold[Response]
    map(f(cf))
  }
}
final class RequestCoproductResponseHandlerFactory[A <: Action, Request, Response <: Coproduct](a: Action, rr: RequestResponse[A]) {
  def apply(actor: ActorRef): RequestCoproductResponseHandler[Request, Response] =
    new RequestCoproductResponseHandler[Request, Response](actor, parse)

  private def parse(msg: Any): Response = {
    rr.parse(msg).
      getOrElse(throw new MatchError(s"Unexpected response (wrong type): $msg to $a")).
      asInstanceOf[Response] //this is safe
  }
}
