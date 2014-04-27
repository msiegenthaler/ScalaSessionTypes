package sst

import scala.language.existentials
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless._
import shapeless.syntax.typeable._

/** Parses send > anyOf(receive*) structures into a request type and a response (coproduct) type. */
@implicitNotFound("Not a request/response: ${A}")
sealed trait RequestResponse[A <: Action] {
  type Request
  type Response <: Coproduct
  def description: String
  def parse(value: Any): Option[Response]
}
object RequestResponse {
  def apply[A <: Action](implicit r: RequestResponse[A]): RequestResponse[A] {type Request = r.Request; type Response = r.Response} = r

  type Aux[A <: Action, Req, Resp <: Coproduct] = RequestResponse[A] {type Request = Req; type Response = Resp}
  implicit def sendReceive[A: ClassTag, R <: Action](implicit r: Response[R]) = new RequestResponse[Then[Send[A], R]] {
    type Request = A
    type Response = r.Out
    def parse(value: Any): Option[Response] = r.parse(value)
    val description = implicitly[ClassTag[A]].toString + " => " + r.description
  }
  implicit def repeated[A <: Action](implicit r: RequestResponse[A]) = new RequestResponse[Repeat[A]] {
    type Request = r.Request
    type Response = r.Response
    def parse(value: Any) = r.parse(value)
    def description = r.description
  }
}

/** Request with only one response variant (send->receive). */
@implicitNotFound("Not a request/single-response: ${A}")
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
