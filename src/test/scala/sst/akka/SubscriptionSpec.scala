package sst.akka

import scala.language.reflectiveCalls
import akka.actor._
import akka.testkit.{TestProbe, TestKit}
import org.specs2.mutable._
import org.specs2.specification.Context
import sst._

class SubscriptionHandlerSpec extends Specification {
  abstract class actors extends TestKit(ActorSystem("SubscriptionHandlerSpec")) with Context with After {
    override def after = system.shutdown()
  }

  case object SubscribePing
  case class PingNotification(count: Int)
  val pingSubscription = send[SubscribePing.type].repeat(receive[PingNotification])

  case object SubscribeIntString
  val intStringSubscription = send[SubscribeIntString.type].repeat(receiveAnyOf[Int, String])

  "SubscriptionHandler" should {
    "support subscriptions with one option in a nice and fluent interface" in new actors {
      val probe = TestProbe()
      val checker = TestProbe()
      val subscriber = system actorOf Props(new Actor {
        val subscription = pingSubscription(probe.ref)
        subscription.activate(SubscribePing)
        override def receive = subscription.handle(n => checker.ref ! s"ping ${n.count} received")

      })
      probe.expectMsg(SubscribePing)

      subscriber ! PingNotification(1)
      checker.expectMsg("ping 1 received")

      subscriber ! PingNotification(2)
      checker.expectMsg("ping 2 received")
    }

    "support more than one message type" in new actors {
      val probe = TestProbe()
      val checker = TestProbe()
      val subscriber = system actorOf Props(new Actor {
        val subscription = intStringSubscription(probe.ref)
        subscription.activate(SubscribeIntString)

        val subscriptionHandler = subscription
          .handle[String](s => checker.ref ! s"Got a string: $s")
          .handle[Int](i => checker.ref ! s"Got an Int: $i")
        override def receive = subscriptionHandler
      })
      probe.expectMsg(SubscribeIntString)

      subscriber ! "Mario"
      checker.expectMsg("Got a string: Mario")

      subscriber ! 123
      checker.expectMsg("Got an Int: 123")
    }

    //TODO unsubscribe

    //TODO watch (maybe as a parameter with default value on activate)
  }
}
