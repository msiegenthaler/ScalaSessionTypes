package sst

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless._

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

  sealed trait Response[A <: Action] {
    type Out <: Coproduct
    def typeable: Typeable[Out]
    def description: String
  }
  object Response {
    @implicitNotFound("Not a response: ${A}")
    type Aux[A <: Action, Out0 <: Coproduct] = Response[A] {type Out = Out0}

    implicit def receive[A: ClassTag](implicit t: Typeable[A :+: CNil]) = new Response[Receive[A]] {
      type Out = A :+: CNil
      def typeable = t
      val description = implicitly[ClassTag[A]].toString
    }
    //TODO does not work for more than to, we'd actually need to do :++:
    implicit def anyOf[A <: Action, B <: Action, O1 <: Coproduct, O2 <: Coproduct](implicit a: Aux[A, O1], b: Aux[B, O2], t: Typeable[O1 :+: O2]) = new Response[AnyOf[A, B]] {
      type Out = O1 :+: O2
      def typeable = t
      val description = a.description + " :+: " + b.description
    }
  }

  @implicitNotFound("Not a request/response: ${A}")
  sealed trait RequestResponse[A <: Action] {
    type Request
    type Response
    def description: String
    protected val responseTypeable: Typeable[Response]
    def exec(actor: ActorRef, req: Request): Response = {
      val resp = actor ? req
      val respUncasted = Coproduct[Any :+: CNil](resp)
      responseTypeable.cast(respUncasted).
        getOrElse(throw new MatchError(s"Unexpected response: $resp"))
    }
  }
  object RequestResponse {
    def apply[A <: Action](implicit R: RequestResponse[A]): RequestResponse[A] {type Request = R.Request; type Response = R.Response} = R

    type Aux[A <: Action, Req, Resp <: Coproduct] = RequestResponse[A] {type Request = Req; type Response = Resp}
    implicit def sendReceive[A: ClassTag, R <: Action](implicit r: Response[R]) = new RequestResponse[Cons[Send[A], R]] {
      val responseTypeable = r.typeable
      type Request = A
      type Response = r.Out
      val description = implicitly[ClassTag[A]].toString + " => " + r.description
    }
  }
}