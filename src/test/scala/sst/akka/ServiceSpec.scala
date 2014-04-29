package sst.akka

import _root_.akka.testkit._
import _root_.akka.actor._
import org.specs2.mutable._
import org.specs2.specification.Context
import sst._

class ServiceSpec extends Specification {
  abstract class actors extends TestKit(ActorSystem("SubscriptionHandlerSpec")) with Context with After {
    override def after = system.shutdown()
  }

  type NumberConverter = ?[String] :>: ![Int]

  "Service" should {
    "provide a fluent interface for single-request -> single-response services" in new actors with ImplicitSender {
      val service = system actorOf Props(new Actor {
        def receive = Service[NumberConverter].handle(_.toInt)
      })

      service ! "12"
      expectMsg(12)
      service ! "-34"
      expectMsg(-34)
    }

    "also work on repeated single-requests" in {
      Service[Repeat[NumberConverter]] must not beNull
    }
  }
}
