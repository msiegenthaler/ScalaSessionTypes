package sst.akka

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import akka.actor._
import akka.testkit._
import akka.util.Timeout
import org.specs2.mutable._
import org.specs2.specification.Context
import sst._

class AskSpec extends Specification {
  abstract class actors extends TestKit(ActorSystem("AskHandlerSpec")) with Context with After {
    override def after = system.shutdown()
  }

  val numberConverter = send[String].receive[Int]
  val safeNumberConverter = send[String].receiveAnyOf[Int, String]

  implicit val timeout: Timeout = 100
  def valueOf[A](f: Future[A]) = Await.result(f, timeout.duration)

  "AskHandler" should {
    "allow action(actor).ask syntax for request/response with one response variant" in new actors {
      val probe = TestProbe()
      val nc = numberConverter(probe.ref)
      val f = nc ask "12"
      probe.expectMsg("12")
      probe.reply(12)
      valueOf(f) must_== 12
    }

    "allow action(actor).map[A](...).map[B](...).ask syntax for request/response with multiple response variant" in new actors {
      "support handling multiple return values" in new actors {
        val probe = TestProbe()
        val nc = safeNumberConverter(probe.ref).
          map[Int](identity).
          map[String] { msg => assert(msg == "not a number"); -1}

        val f1 = nc ask "x1"
        probe.expectMsg("x1")
        probe.reply("not a number")
        valueOf(f1) must_== -1

        val f2 = nc ask "123"
        probe.expectMsg("123")
        probe.reply(123)
        valueOf(f2) must_== 123
      }
    }
  }
}