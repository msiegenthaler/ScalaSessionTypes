package sst

import org.specs2.mutable._
import shapeless.test.illTyped

class OppositeSpec extends Specification {
  private def assertCompiled = true must beTrue

  "Opposite[]" should {
    "be Receive[String] for Send[String]" in {
      type In = ![String]
      type Op = ?[String]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
    "be Send[String] for Receive[String]" in {
      type In = ?[String]
      type Op = ![String]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }

    "be (![String] :>: ?[Int]) for (?[String] :>: ![Int]" in {
      type In = ?[String] :>: ![Int]
      type Op = ![String] :>: ?[Int]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
    "be (?[String] :>: ![Int]) for (![String] :>: ?[Int]" in {
      type In = ![String] :>: ?[Int]
      type Op = ?[String] :>: ![Int]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }

    "be Choice[![String],![Int]] for AnyOf[?[String],?[Int]" in {
      type In = ?[String] :&: ?[Int]
      type Op = ![String] :@: ![Int]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
    "be AnyOf[?[String],?[Int] for Choice[![String],![Int]]" in {
      type In = ![String] :@: ![Int]
      type Op = ?[String] :&: ?[Int]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
    "be AnyOf[?[String],AnyOf[?[Int],?[Long]] for Choice[![String],Choice[![Int],![Long]]" in {
      type In = ![String] :@: ![Int] :@: ![Long]
      type Op = ?[String] :&: ?[Int] :&: ?[Long]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
    "be Choice[![String],Choice[![Int],![Long]] for AnyOf[?[String],AnyOf[?[Int],?[Long]]" in {
      type In = ?[String] :&: ?[Int] :&: ?[Long]
      type Op = ![String] :@: ![Int] :@: ![Long]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }

    "be Repeat[![String]] for Repeat[?[String]]" in {
      type In = Repeat[?[String]]
      type Op = Repeat[![String]]
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
    "be Break for Break" in {
      type In = Break
      type Op = Break
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
    "be (![String] :>: Break) for (?[String] :>: Break)" in {
      type In = ?[String] :>: Break
      type Op = ![String] :>: Break
      val op: Op = Opposite[In]
      implicitly[Op =:= op.Type]
      op must not beNull
    }
  }

  "Opposite.is" should {
    "be true for ?[String] and ![String]" in {
      type A = ?[String]
      type B = ![String]
      Opposite.is[A, B]
      assertCompiled
    }
    "be false for ?[String] and ?[String]" in {
      type A = ?[String]
      type B = ?[String]
      illTyped("Opposite.is[A,B]")
      assertCompiled
    }
    "be true for AnyOf[?[String],?[Int] and Choice[![String],![Int]]" in {
      type A = AnyOf[?[String], ?[Int]]
      type B = Choice[![String], ![Int]]
      Opposite.is[A, B]
      assertCompiled
    }
    "be false for AnyOf[?[String],?[Int] and Choice[?[String],?[Int]]" in {
      type A = AnyOf[?[String], ?[Int]]
      type B = Choice[?[String], ?[Int]]
      illTyped("Opposite.is[A,B]")
      assertCompiled
    }
    "be false for AnyOf[?[String],?[Int] and AnyOf[?[String],?[Int]]" in {
      type A = AnyOf[?[String], ?[Int]]
      type B = AnyOf[?[String], ?[Int]]
      illTyped("Opposite.is[A,B]")
      assertCompiled
    }
    "be false for AnyOf[?[String],?[Int] and AnyOf[![String],![Int]]" in {
      type A = AnyOf[?[String], ?[Int]]
      type B = AnyOf[![String], ![Int]]
      illTyped("Opposite.is[A,B]")
      assertCompiled
    }
  }

  "Opposite of opposite" should {
    "be original for Receive[String]" in {
      type T = ?[String]
      val r = Opposite[T]
      val o = Opposite[r.Type]
      implicitly[T =:= o.Type]
      assertCompiled
    }
    "be original for Send[String]" in {
      type T = ![String]
      val r = Opposite[T]
      val o = Opposite[r.Type]
      implicitly[T =:= o.Type]
      assertCompiled
    }
    "be original for ![String] :>: ?[Int]" in {
      type T = ![String] :>: ?[Int]
      val r = Opposite[T]
      val o = Opposite[r.Type]
      implicitly[T =:= o.Type]
      assertCompiled
    }
    "be original for ?[String] :>: (![Int] :@: ![Long] :@: ![Double])" in {
      type T = ?[String] :>: (![Int] :@: ![Long] :@: ![Double])
      val r = Opposite[T]
      val o = Opposite[r.Type]
      implicitly[T =:= o.Type]
      assertCompiled
    }
  }
}
