package sst

import shapeless._
import org.specs2.mutable._

class SubscriptionSpec extends Specification {
  private def isOk() = true must beTrue

  "Subscription" should {
    "parse ![String] :>: Repeat[?[Int]] into Setup=String, Message=Int :+: CNil" in {
      val s = Subscription[![String] :>: Repeat[?[Int]]]
      implicitly[s.Setup =:= String]
      implicitly[s.Message =:= (Int :+: CNil)]
      isOk()
    }
    "parse ![Int] :>: Repeat[?[String] :&: ?[Long]] into Setup=Int, Message=String :+: Long :+: CNil" in {
      val s = Subscription[![Int] :>: Repeat[?[String] :&: ?[Long]]]
      implicitly[s.Setup =:= Int]
      implicitly[s.Message =:= (String :+: Long :+: CNil)]
      isOk()
    }
  }

  "Subscription[![Int] :>: Repeat[?[String] :&: ?[Long]]].parse" should {
    val s = Subscription[![Int] :>: Repeat[?[String] :&: ?[Long]]]
    "be defined for Strings" in {
      s.parse.isDefinedAt("hello") must beTrue
      s.parse.isDefinedAt("") must beTrue
      s.parse.isDefinedAt("Mario") must beTrue
    }
    "be defined for Longs" in {
      s.parse.isDefinedAt(0L) must beTrue
      s.parse.isDefinedAt(1L) must beTrue
      s.parse.isDefinedAt(-1L) must beTrue
      s.parse.isDefinedAt(2L) must beTrue
      s.parse.isDefinedAt(1231341113L) must beTrue
    }
    "be undefined for Ints" in {
      s.parse.isDefinedAt(0) must beFalse
      s.parse.isDefinedAt(1) must beFalse
    }

    "return Coproduct(\"hello\") for String hello" in {
      s.parse("hello") must_== Coproduct[String :+: Long :+: CNil]("hello")
    }
    "return Coproduct(2L) for Long 2" in {
      s.parse(2L) must_== Coproduct[String :+: Long :+: CNil](2L)
    }
  }

  "Subscription.description" should {
    "be 'Int subscribes to java.lang.String' for ![Int] :>: Repeat[?[String]]" in {
      Subscription[![Int] :>: Repeat[?[String]]].description must_== "Int subscribes to java.lang.String"
    }
    "be 'Int subscribes to java.lang.String or Long' for ![Int] :>: Repeat[?[String] :&: ?[Long]]" in {
      Subscription[![Int] :>: Repeat[?[String] :&: ?[Long]]].description must_== "Int subscribes to java.lang.String or Long"
    }
  }

  "SingleNotificationSubscription" should {
    "be PartialFunction[Any, String] for ![Int] :>: Repeat[?[String]]" in {
      val s = SingleNotificationSubscription[![Int] :>: Repeat[?[String]]]
      implicitly[s.Setup =:= Int]
      implicitly[s.Message =:= String]
      s.parse.lift("Hi") must beSome("Hi")
      s.parse.lift(123) must beNone
    }
    "be PartialFunction[Any, Int] for ![String] :>: Repeat[?[Int]]" in {
      val s = SingleNotificationSubscription[![String] :>: Repeat[?[Int]]]
      implicitly[s.Setup =:= String]
      implicitly[s.Message =:= Int]
      s.parse.lift("Hi") must beNone
      s.parse.lift(123) must beSome(123)
    }
  }
}
