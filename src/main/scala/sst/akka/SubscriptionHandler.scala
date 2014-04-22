package sst.akka

import akka.actor._
import sst._
import sst.utils.{CoproductHandlerIsRunnable, CoproductHandler}
import shapeless.Coproduct
import sst.utils.CoproductOps.{Contains, Remove}
import shapeless.ops.coproduct.Selector

final class SubscriptionHandler(actor: ActorRef) {
  def subscription[A <: Action](implicit s: Subscription[A]) = new Container[A, s.Setup, s.Message](s).initial

  final class Container[A <: Action, Setup, Message <: Coproduct](s: Subscription[A]) {
    private[SubscriptionHandler] def initial = new ResponseHandler[Message](CoproductHandler[Message])

    final class ResponseHandler[Remaining <: Coproduct] private[Container](handler: CoproductHandler[Message, Remaining, _]) {
      def handle[A] = new AnyRef {
        def apply[B](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[Message, A]) = {
          val h = handler.handle(f.andThen(_ => ()))
          new ResponseHandler(h)
        }
      }

      def receive(implicit r: CoproductHandlerIsRunnable[Remaining]) = s.parse.andThen { msg_ =>
        val msg = msg_.asInstanceOf[Message]
        handler.handler(msg)
        ()
      }

      def activate(subscriber: ActorRef, setupMsg: Setup)(implicit r: CoproductHandlerIsRunnable[Remaining]): Unit =
        actor.!(setupMsg)(subscriber)
    }
  }
}