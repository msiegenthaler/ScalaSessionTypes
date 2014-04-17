package sst

import scala.language.existentials
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless._
import syntax.typeable._
import shapeless.ops.coproduct._


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
}

sealed trait Response[A <: Action] {
  type Out <: Coproduct
  def parse(value: Any): Option[Out] = parts.view.flatMap(_.parser(value)).headOption
  def description: String = parts.map(_.tag.toString).mkString(" :+: ")
  protected type Parser = Any => Option[Out]
  protected case class Part(parser: Parser, tag: ClassTag[_])
  protected def parts: List[Part]
  protected def part[X: Typeable : ClassTag](implicit i: Inject[Out, X]) = {
    val p = (value: Any) => value.cast[X] map (Coproduct[Out](_))
    Part(p, implicitly[ClassTag[X]])
  }
}
object Response {
  @implicitNotFound("Not a response: ${A}")
  type Aux[A <: Action, Out0 <: Coproduct] = Response[A] {type Out = Out0}

  implicit def receive[A: ClassTag : Typeable] = new Response[Receive[A]] {
    type Out = A :+: CNil
    val parts = part[A] :: Nil
  }
  implicit def anyOfLeft[A: ClassTag : Typeable, B <: Action](implicit br: Response[B]) = new Response[AnyOf[Receive[A], B]] {
    type Out = A :+: br.Out
    val parts: List[Part] = part[A] :: br.parts.map { part =>
      def parser(in: Any): Option[Out] = part.parser(in).map(Inr(_))
      Part(parser, part.tag)
    }
  }
}
