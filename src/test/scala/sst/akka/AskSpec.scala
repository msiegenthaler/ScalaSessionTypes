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
import sst.utils._
import sst.akka._
import shapeless._

class AskSpec extends Specification {
  abstract class actors extends TestKit(ActorSystem("AskHandlerSpec")) with Context with After {
    override def after = system.shutdown()
  }

  val numberConverter = send[String].receive[Int]
  val numberConverterR = send[String].receive[Int].repeat
  val safeNumberConverter = send[String].receiveAnyOf[Int, String]

  implicit val timeout: Timeout = 100
  def valueOf[A](f: Future[A]) = Await.result(f, timeout.duration)

  "action(actor)" should {
    "allow ask syntax for request/response with one response variant" in new actors {
      val probe = TestProbe()
      val nc = numberConverter(probe.ref)
      val f = nc ask "12"
      probe.expectMsg("12")
      probe.reply(12)
      valueOf(f) must_== 12
    }
    "allow ask syntax for repeated request/response with one response variant" in new actors {
      val probe = TestProbe()
      val nc = numberConverterR(probe.ref)
      val f = nc ask "42"
      probe.expectMsg("42")
      probe.reply(42)
      valueOf(f) must_== 42
    }

    "allow map(...).ask syntax for request/response with multiple response variant" in new actors {
      val probe = TestProbe()
      val mapIS = CoproductFold[Int :+: String :+: CNil].
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1}
      val nc = safeNumberConverter(probe.ref).map(mapIS)

      val f1 = nc ask "x1"
      probe.expectMsg("x1")
      probe.reply("not a number")
      valueOf(f1) must_== -1

      val f2 = nc ask "123"
      probe.expectMsg("123")
      probe.reply(123)
      valueOf(f2) must_== 123
    }
    "allow mapCoproduct(...).ask syntax for request/response with multiple response variant" in new actors {
      val probe = TestProbe()
      val mapIS = CoproductFold[Int :+: String :+: CNil].
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1}
      val nc = safeNumberConverter(probe.ref).mapCoproduct(_.
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1})

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