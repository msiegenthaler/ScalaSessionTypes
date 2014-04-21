package sst

import org.specs2.mutable._
import shapeless._
import shapeless.test.illTyped

class RequestResponseSpec extends Specification {
  private def isOk() = true must beTrue

  "RequestResponse" should {
    "parse ![String] :>: ?[Int] to Request=String, Response=Int :+: CNil" in {
      val r = RequestResponse[![String] :>: ?[Int]]
      implicitly[r.Request =:= String]
      implicitly[r.Response =:= (Int :+: CNil)]
      isOk()
    }
    "parse ![String] :>: (?[Int] :&: ?[String]) to Request=String, Response=Int :+: String :+: CNil" in {
      val r = RequestResponse[![String] :>: (?[Int] :&: ?[String])]
      implicitly[r.Request =:= String]
      implicitly[r.Response =:= (Int :+: String :+: CNil)]
      isOk()
    }

    "not parse ![String]" in {
      illTyped("RequestResponse[![String]]")
      isOk()
    }
    "not parse ![String] :>: ![Int]" in {
      illTyped("RequestResponse[![String] :>: ![Int]]")
      isOk()
    }
    "not parse ?[String]" in {
      illTyped("RequestResponse[?[String]]")
      isOk()
    }
    "not parse ?[String] :>: ![Int]" in {
      illTyped("RequestResponse[?[String] :>: ![Int]]")
      isOk()
    }
  }

}
