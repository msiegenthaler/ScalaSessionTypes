package sst.akka

import scala.language.implicitConversions
import akka.actor._
import shapeless._
import shapeless.syntax.typeable._
import sst.Action
import sst.utils.CoproductOps._

/** ActorRef wrapper for subscription actions. */
abstract class SubscriptionActorRef[A <: Action, S, M](val action: Action, val actor: ActorRef) {
  type Setup = S
  type Message = M

  /** Start the subscription. */
  def activate(setupMsg: Setup)(implicit subscriber: ActorRef) = actor ! setupMsg

  /**
   * Construct a receive (partial) function for this subscription.
   * Usage: def receive = subscr.handle(..) orElse { ... }
   */
  def handle[X](f: Message => X): PartialFunction[Any, Unit]

  override def toString = s"$actor as $action"
}

abstract class CoproductSubscriptionActorRef[A <: Action, S, M <: Coproduct](action: Action, actor: ActorRef)
  extends SubscriptionActorRef[A, S, M](action, actor) with SubscriptionHandler[M, M] {

}


trait SubscriptionHandler[Message <: Coproduct, Remaining <: Coproduct] {
  protected[akka] val function: PartialFunction[Any, Unit]
}
object SubscriptionHandler extends LowPrioritySubscriptionHandler {
  implicit def last[M <: Coproduct, L](h: SubscriptionHandler[M, L :+: CNil]) =
    new SubscriptionHandlerLast[M, L](h)
}
trait LowPrioritySubscriptionHandler {
  implicit def more[M <: Coproduct, R <: Coproduct](h: SubscriptionHandler[M, R]) = new SubscriptionHandlerMore[M, R](h)

  final class SubscriptionHandlerMore[Message <: Coproduct, Remaining <: Coproduct](handler: SubscriptionHandler[Message, Remaining]) {
    def handlerFor[A: Typeable](f: A => Unit)(implicit r: Remove[Remaining, A]): SubscriptionHandler[Message, r.Out] = {
      new SubscriptionHandler[Message, r.Out] {
        val function = handler.function orElse handlerFunction[A](f)
      }
    }
  }
  final class SubscriptionHandlerLast[Message <: Coproduct, Last](handler: SubscriptionHandler[Message, Last :+: CNil]) {
    def handlerFor[A >: Last : Typeable](f: A => Unit): PartialFunction[Any, Unit] =
      handler.function orElse handlerFunction[A](f)
  }
  private def handlerFunction[A: Typeable](f: A => Unit): PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
    def isDefinedAt(x: Any) = x.cast[A].isDefined
    def apply(x: Any) = f(x.cast[A].get)
  }
}