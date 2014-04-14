package sst

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless._

object ActorIntegration {
  sealed trait Request[A <: Action] {
    type Out <: Coproduct
    def description: String
  }
  object Request {
    @implicitNotFound("Not a response: ${A}")
    type Aux[A <: Action, Out0 <: Coproduct] = Request[A] {type Out = Out0}

    implicit def receive[A: ClassTag] = new Request[Receive[A]] {
      type Out = A :+: CNil
      val description = implicitly[ClassTag[A]].toString
    }
    //TODO does not work for more than to, we'd actually need to do :++:
    implicit def anyOf[A <: Action, B <: Action, O1 <: Coproduct, O2 <: Coproduct](implicit a: Aux[A, O1], b: Aux[B, O2]) = new Request[AnyOf[A, B]] {
      type Out = O1 :+: O2
      val description = a.description + " :+: " + b.description
    }
  }

  @implicitNotFound("Not a request/response: ${A}")
  sealed trait RequestResponse[A <: Action] {
    type Request
    type Response <: Coproduct
    def description: String
  }
  object RequestResponse {
    def apply[A <: Action](implicit R: RequestResponse[A]): RequestResponse[A] {type Request = R.Request; type Response = R.Response} = R

    type Aux[A <: Action, Req, Resp <: Coproduct] = RequestResponse[A] {type Request = Req; type Response = Resp}
    implicit def sendReceive[A: ClassTag, R <: Action](implicit r: Request[R]) = new RequestResponse[Cons[Send[A], R]] {
      type Request = A
      type Response = r.Out
      val description = implicitly[ClassTag[A]].toString + " => " + r.description
    }
  }
}
