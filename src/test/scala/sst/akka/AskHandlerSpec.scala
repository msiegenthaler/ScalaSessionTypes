package sst.akka

import scala.language.reflectiveCalls
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import akka.actor._
import akka.testkit._
import akka.util.Timeout
import org.specs2.mutable._
import org.specs2.specification.Context
import sst._

class AskHandlerSpec extends Specification {
  abstract class actors extends TestKit(ActorSystem("AskHandlerSpec")) with Context with After {
    override def after = system.shutdown()
  }

  type NumberConverter = ![String] :>: ?[Int]
  val safeNumberConverter = send[String].receiveAnyOf[Int, String]

  implicit val timeout: Timeout = 100
  def valueOf[A](f: Future[A]) = Await.result(f, timeout.duration)

  "AskHandler" should {
    "allow actor.ask[Action].handle...send syntax" in new actors {
      val probe = TestProbe()
      val nc = probe.ref.ask[NumberConverter].handle[Int](identity)
      val f = nc.send("12")
      probe.expectMsg("12")
      probe.reply(12)
      valueOf(f) must_== 12
    }

    "support handling multiple return values" in new actors {
      val probe = TestProbe()
      val nc = probe.ref.ask[safeNumberConverter.Type].
        handle[Int](identity).
        handle[String] { msg =>
        assert(msg == "not a number")
        -1
      }

      val f1 = nc.send("x1")
      probe.expectMsg("x1")
      probe.reply("not a number")
      valueOf(f1) must_== -1

      val f2 = nc.send("123")
      probe.expectMsg("123")
      probe.reply(123)
      valueOf(f2) must_== 123
    }
  }
}
