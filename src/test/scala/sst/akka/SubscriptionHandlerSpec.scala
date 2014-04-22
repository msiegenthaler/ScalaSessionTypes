package sst.akka

import scala.language.reflectiveCalls
import akka.actor._
import akka.testkit.{TestProbe, TestKit}
import org.specs2.mutable._
import org.specs2.specification.Context
import sst._

class SubscriptionHandlerSpec extends Specification {
  abstract class actors extends TestKit(ActorSystem("AskHandlerSpec")) with Context with After {
    override def after = system.shutdown()
  }

  case object SubscribePing
  case class PingNotification(count: Int)
  type PingSubscription = ![SubscribePing.type] :>: Repeat[?[PingNotification]]

  "SubscriptionHandler" should {
    "support subscriptions with one option in a nice and fluent interface" in new actors {
      val probe = TestProbe()
      val checker = TestProbe()
      val subscriber = system actorOf Props(new Actor {
        val subscription = probe.ref.subscription[PingSubscription].
          handle[PingNotification](n => checker.ref ! s"ping ${n.count} received")
        subscription.activate(context.self, SubscribePing)
        override def receive = subscription.receive
      })
      probe.expectMsg(SubscribePing)

      subscriber ! PingNotification(1)
      checker.expectMsg("ping 1 received")

      subscriber ! PingNotification(2)
      checker.expectMsg("ping 2 received")
    }

    //TODO unsubscribe

    //TODO watch (maybe as a parameter with default value on activate)

    //TODO multiple handlers (multiple types to receive)
  }
}
