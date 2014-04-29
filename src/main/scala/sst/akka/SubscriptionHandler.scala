package sst.akka

import akka.actor._
import shapeless._
import sst._

final class SingleNotificationSubscriptionHandler[A <: Action, Setup, Message](action: Action, sns: SingleNotificationSubscription[A]) {
  def apply(actor: ActorRef): SubscriptionActorRef[A, Setup, Message] = {
    new SubscriptionActorRef[A, Setup, Message](action, actor) {
      def handle[X](f: Function1[Message, X]) = sns.parse andThen { msg =>
        f(msg.asInstanceOf[Message]) //cast is safe
        ()
      }
    }
  }
}

final class CoproductNotificationSubscriptionHandler[A <: Action, Setup, Message <: Coproduct](action: Action, s: Subscription[A]) {
  def apply(actor: ActorRef): CoproductSubscriptionActorRef[A, Setup, Message] = {
    new CoproductSubscriptionActorRef[A, Setup, Message](action, actor) {
      val function = PartialFunction.empty
      def handle[X](f: Function1[Message, X]) = s.parse andThen { msg =>
        f(msg.asInstanceOf[Message]) //cast is safe
        ()
      }
    }
  }
}