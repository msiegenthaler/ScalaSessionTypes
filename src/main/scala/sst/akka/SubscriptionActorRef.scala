package sst.akka

import scala.concurrent._
import akka.actor._
import akka.util.Timeout
import sst.Action

/** ActorRef wrapper for subscription actions. */
abstract class SubscriptionActorRef[A <: Action, -Setup, +Message](val action: Action, val actor: ActorRef) {
  /** Start the subscription. */
  def activate(setupMsg: Setup)(implicit subscriber: ActorRef) = actor ! setupMsg

  def handle[X](f: Message => X): PartialFunction[Any, Unit]

  override def toString = s"$actor as $action"
}