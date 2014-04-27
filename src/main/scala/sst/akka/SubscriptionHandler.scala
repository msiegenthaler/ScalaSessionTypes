package sst.akka

import akka.actor._
import sst._
import sst.utils.{CoproductMapperIsComplete, CoproductMapper}
import shapeless.Coproduct
import sst.utils.CoproductOps.{Contains, Remove}
import shapeless.ops.coproduct.Selector

/**
 * Subscription pattern for session types.
 *
 * Usage syntax where IntSource is a subscription session type:
 * <code>
 * class MyActor(intSource: ActorRef) extends Actor {
 * var counter = 0
 *
 * val nsub = intSource.subscription[IntSource].handle[Int](int => counter = counter + 1)
 * nsub.activate(SubscribeToInts)
 *
 * def receive = nsub.receive orElse {
 * case name: String => println(s"Hi $name, I counted $counter ints")
 * }
 * <code>
 */
final class SubscriptionHandler(actor: ActorRef) {
  def subscription[A <: Action](implicit s: Subscription[A]) = new Container[A, s.Setup, s.Message](s).initial

  final class Container[A <: Action, Setup, Message <: Coproduct](s: Subscription[A]) {
    private[SubscriptionHandler] def initial = new ResponseHandler[Message](CoproductMapper[Message])

    final class ResponseHandler[Remaining <: Coproduct] private[Container](handler: CoproductMapper[Message, Remaining, _]) {
      def handle[A] = new AnyRef {
        def apply[B](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[Message, A]) = {
          val h = handler.map(f.andThen(_ => ()))
          new ResponseHandler(h)
        }
      }

      def receive(implicit r: CoproductMapperIsComplete[Remaining]) = s.parse.andThen { msg_ =>
        val msg = msg_.asInstanceOf[Message]
        handler(msg)
        ()
      }

      def activate(setupMsg: Setup)(implicit subscriber: ActorRef, r: CoproductMapperIsComplete[Remaining]): Unit =
        actor.!(setupMsg)(subscriber)
    }
  }
}