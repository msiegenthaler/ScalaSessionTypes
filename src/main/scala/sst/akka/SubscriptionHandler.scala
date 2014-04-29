package sst.akka

import akka.actor._
import akka.util.Timeout
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