package sst.akka

import akka.actor._
import sst.Action

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