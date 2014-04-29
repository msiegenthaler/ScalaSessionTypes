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

  type IS = Int :+: String :+: CNil

  val numberConverter = send[String].receive[Int]
  val numberConverterR = send[String].receive[Int].repeat
  val safeNumberConverter = send[String].receiveAnyOf[Int, String]

  implicit val timeout: Timeout = 100
  def valueOf[A](f: Future[A]) = Await.result(f, timeout.duration)

  "action(actor)" should {
    "allow ask syntax for request/response with one response variant" in new actors {
      val probe = TestProbe()
      val nc = numberConverter(probe.ref)
      val f: Future[Int] = nc ask "12"
      probe.expectMsg("12")
      probe.reply(12)
      valueOf(f) must_== 12
    }
    "allow ask syntax for repeated request/response with one response variant" in new actors {
      val probe = TestProbe()
      val nc = numberConverterR(probe.ref)
      val f: Future[Int] = nc ask "42"
      probe.expectMsg("42")
      probe.reply(42)
      valueOf(f) must_== 42
    }
    "allow map before ask syntax for repeated request/response with one response variant" in new actors {
      val probe = TestProbe()
      val nc = numberConverterR(probe.ref) map (_.toLong + 1)
      val f: Future[Long] = nc ask "42"
      probe.expectMsg("42")
      probe.reply(42)
      valueOf(f) must_== 43L
    }

    "allow ask for coproduct with multiple response variant" in new actors {
      val probe = TestProbe()
      val nc = safeNumberConverter(probe.ref)

      val f1 = nc ask "x1"
      probe.expectMsg("x1")
      probe.reply("not a number")
      valueOf(f1) must_== Coproduct[IS]("not a number")

      val f2 = nc ask "123"
      probe.expectMsg("123")
      probe.reply(123)
      valueOf(f2) must_== Coproduct[IS](123)
    }

    "provide an standard way to map coproducts of the future" in new actors {
      val probe = TestProbe()
      val nc = safeNumberConverter(probe.ref)

      val mapIS = CoproductFold[IS].
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1}

      val f1 = nc ask "x1"
      probe.expectMsg("x1")
      probe.reply("not a number")
      val f1m: Future[Int] = f1.map(mapIS)
      valueOf(f1m) must_== -1

      val f2 = nc ask "123"
      val f2m: Future[Int] = f2.map(mapIS)
      probe.expectMsg("123")
      probe.reply(123)
      valueOf(f2m) must_== 123
    }
    "provide an easy way to map coproducts of the future" in new actors {
      val probe = TestProbe()
      val nc = safeNumberConverter(probe.ref)

      val f1 = nc ask "x1"
      val f1m: Future[Int] = f1.mapCoproduct(_.
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1}
      )
      probe.expectMsg("x1")
      probe.reply("not a number")
      valueOf(f1m) must_== -1

      val f2 = nc ask "123"
      val f2m: Future[Int] = f2.mapCoproduct(_.
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1}
      )
      probe.expectMsg("123")
      probe.reply(123)
      valueOf(f2m) must_== 123
    }

    "allow map before ask for request/response with multiple response variant" in new actors {
      val probe = TestProbe()
      val mapIS = CoproductFold[IS].
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1}
      val nc = safeNumberConverter(probe.ref).map(mapIS)

      val f1: Future[Int] = nc ask "x1"
      probe.expectMsg("x1")
      probe.reply("not a number")
      valueOf(f1) must_== -1

      val f2: Future[Int] = nc ask "123"
      probe.expectMsg("123")
      probe.reply(123)
      valueOf(f2) must_== 123
    }

    "allow mapCoproduct before ask for request/response with multiple response variant" in new actors {
      val probe = TestProbe()
      val nc = safeNumberConverter(probe.ref).mapCoproduct(_.
        fold[Int](identity).
        fold[String] { msg => assert(msg == "not a number"); -1})

      val f1: Future[Int] = nc ask "x1"
      probe.expectMsg("x1")
      probe.reply("not a number")
      valueOf(f1) must_== -1

      val f2: Future[Int] = nc ask "123"
      probe.expectMsg("123")
      probe.reply(123)
      valueOf(f2) must_== 123
    }
  }
}