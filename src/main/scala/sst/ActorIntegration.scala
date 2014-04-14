package sst

import scala.reflect.ClassTag
import scala.annotation.implicitNotFound
import shapeless._

object ActorIntegration {
  def witness[A]: A = null.asInstanceOf[A]

  sealed class ResponseType[T <: Coproduct](tags: ClassTag[_]*) {
    type Type = T
    def describe = tags.map(_.toString).mkString(" :+: ")
    override def toString = describe
  }
  object ResponseType {
    def _1[A: ClassTag]: ResponseType[A :+: CNil] = new ResponseType(tag[A])
    def _2[A: ClassTag, B: ClassTag]: ResponseType[A :+: B :+: CNil] = new ResponseType(tag[A], tag[B])
    protected def tag[A: ClassTag] = implicitly[ClassTag[A]]
  }

  sealed class ResponseComposer[A <: Action, R <: ResponseType[T], T <: Coproduct](val response: R) {
    type Response = R
    type Type = T
  }
  implicit def rcReceive[A: ClassTag]: ResponseComposer[Receive[A], ResponseType[A :+: CNil], A :+: CNil] = new ResponseComposer(ResponseType._1[A])
  implicit def rcAnyOf2[A: ClassTag, B: ClassTag]: ResponseComposer[AnyOf[Receive[A], Receive[B]], ResponseType[A :+: B :+: CNil], A :+: B :+: CNil] =
    new ResponseComposer(ResponseType._2[A, B])
  //TODO for more than 2...


  @implicitNotFound("Not a request/response ${A}")
  final class RRComposer[A, Request, Response](val response2: Response) {
    type Req = Request
    type Resp = Response
    val response: Resp = response2
  }
  implicit def rr[Req, Resp <: Action](implicit rc: ResponseComposer[Resp, _, _]): RRComposer[Cons[Send[Req], Resp], Req, rc.Response] =
    new RRComposer(rc.response.asInstanceOf[rc.Response])

  final class RequestResponse[Request, Response](val response: Response) {
    def check(request: Request, response: Response) = ()
    def description(implicit request: ClassTag[Request]) =
      request.toString + " => " + response.toString
  }

  implicit def requestResponse[A <: Action](implicit rrc: RRComposer[A, _, _]): RequestResponse[rrc.Req, rrc.Resp] =
    new RequestResponse(rrc.response)
}
