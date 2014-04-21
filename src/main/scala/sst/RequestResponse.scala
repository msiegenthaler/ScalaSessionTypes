package sst

import scala.language.existentials
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless._

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