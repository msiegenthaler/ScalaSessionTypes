package sst

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless._
import shapeless.syntax.typeable._

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

/** Subscription with only one notification message. */
@implicitNotFound("Not a single notification subscription: ${A}")
trait SingleNotificationSubscription[A <: Action] {
  type Setup
  type Message
  def parse: PartialFunction[Any, Message]
}
object SingleNotificationSubscription {
  def apply[A <: Action](implicit s: SingleNotificationSubscription[A]): Aux[A, s.Setup, s.Message] = s
  type Aux[A <: Action, S, M] = SingleNotificationSubscription[A] {type Setup = S; type Message = M}

  implicit def sendRepeatedReceive[S, M: Typeable]: Aux[![S] :>: Repeat[?[M]], S, M] = new SingleNotificationSubscription[![S] :>: Repeat[?[M]]] {
    type Setup = S
    type Message = M
    def parse = {
      case msg if msg.cast[M].isDefined => msg.cast[M].get
    }
  }
}
