package sst

import org.specs2.mutable._
import shapeless._
import shapeless.test.illTyped

class ResponseSpec extends Specification {
  private def isOk() = true must beTrue

  "Response" should {
    "parse Receive[String] into String :+: CNil" in {
      val r = Response[?[String]]
      implicitly[(String :+: CNil) =:= r.Out]
      isOk()
    }
    "parse ?[String] :&: ?[Int] into String :+: Int :+: CNil" in {
      val r = Response[?[String] :&: ?[Int]]
      implicitly[(String :+: Int :+: CNil) =:= r.Out]
      isOk()
    }
    "parse ?[String] :&: ?[Int] :&: ?[Long] into String :+: Int :+: Long :+: CNil" in {
      val r = Response[?[String] :&: ?[Int] :&: ?[Long]]
      implicitly[(String :+: Int :+: Long :+: CNil) =:= r.Out]
      isOk()
    }
    "parse (?[String] :&: ?[Int]) :&: ?[Long] into Long :+: String :+: Int :+: CNil" in {
      val r = Response[(?[String] :&: ?[Int]) :&: ?[Long]]
      implicitly[(Long :+: String :+: Int :+: CNil) =:= r.Out]
      isOk()
    }
    "parse ?[String] :&: (?[Int] :&: ?[Long]) into String :+: Int :+: Long :+: CNil" in {
      val r = Response[?[String] :&: (?[Int] :&: ?[Long])]
      implicitly[(String :+: Int :+: Long :+: CNil) =:= r.Out]
      isOk()
    }

    "not accept ![String]" in {
      type T = ![String]
      illTyped("Response[T]")
      isOk()
    }
    "not accept ?[String] :@: ?[Int]" in {
      type T = ?[String] :@: ?[Int]
      illTyped("Response[T]")
      isOk()
    }
  }

  "Response[?[String] :&: ?[Long]].parse" should {
    val r = Response[?[String] :&: ?[Long]]
    "return some Coproduct(\"hello\") for String hello" in {
      r.parse("hello") must_== Some(Coproduct[String :+: Long :+: CNil]("hello"))
    }
    "return some Coproduct(2L) for Long 2" in {
      r.parse(2L) must_== Some(Coproduct[String :+: Long :+: CNil](2L))
    }
    "be None for Ints" in {
      r.parse(1) must beNone
      r.parse(-1) must beNone
    }
  }
}
