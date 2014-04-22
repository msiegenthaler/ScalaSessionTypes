package sst

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import shapeless._
import scala.reflect.ClassTag

/** Parses send > repeat(anyOf(receive*)) structures into a setup type and a notification message (coproduct) type. */
@implicitNotFound("Not a subscription: ${A}")
sealed trait Subscription[A <: Action] {
  type Setup
  type Message <: Coproduct
  def parse: PartialFunction[Any, Message]
  def description: String
}

object Subscription {
  def apply[A <: Action](implicit s: Subscription[A]): Aux[A, s.Setup, s.Message] = s

  type Aux[A <: Action, S, M <: Coproduct] = Subscription[A] {type Setup = S; type Message = M}
  implicit def sendRepeatedReceive[S: ClassTag, R <: Action](implicit r: Response[R]): Aux[Then[Send[S], Repeat[R]], S, r.Out] = new Subscription[Then[Send[S], Repeat[R]]] {
    type Setup = S
    type Message = r.Out
    def description = implicitly[ClassTag[S]].toString + " subscribes to " + r.description
    def parse = {
      case msg if r.parse(msg).isDefined =>
        r.parse(msg).get
    }
  }
}