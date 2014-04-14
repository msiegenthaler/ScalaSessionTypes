package sst

import shapeless._
import RequestResponse._
import Handle._

object ActorIntegration {
  //TODO This is a stub..
  class ActorRef(id: String, body: Any => Any = _ => ()) {
    def !(msg: Any): Unit = {
      println(s"Sent $msg to $id")
    }
    def ?(msg: Any): Any = {
      val resp = body(msg)
      println(s"Sent $msg to $id and got $resp")
      resp
    }
  }

  class ActorRequestResponse[A <: Action, Request, Response <: Coproduct](val rr: RequestResponse[A]) {
    def description = rr.description
    def exec(actor: ActorRef, req: Request): Response = {
      val resp = actor ? req
      rr.parse(resp).
        getOrElse(throw new MatchError(s"Unexpected response: $resp"))
        .asInstanceOf[Response]
    }
    def handle(actor: ActorRef, request: Request) = {
      val response = exec(actor, request)
      Handler(response)
    }
  }

  def apply[A <: Action](implicit rr: RequestResponse[A]) = new ActorRequestResponse[A, rr.Request, rr.Response](rr)
}