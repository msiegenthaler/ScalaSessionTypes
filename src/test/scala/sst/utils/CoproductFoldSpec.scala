package sst.utils

import org.specs2.mutable._
import shapeless._
import shapeless.test.illTyped

class CoproductFoldSpec extends Specification {
  type S = String :+: CNil
  type SI = String :+: Int :+: CNil
  type SIL = String :+: Int :+: Long :+: CNil
  val sSI = Coproduct[SI]("Mario")

  "CoproductFold" should {
    "allow typesafe handling of all cases for single element coproduct" in {
      val handler: S => String = CoproductFold[S].fold[String](identity)
      handler(Coproduct[S]("Mario")) must_== "Mario"
    }
    "allow typesafe handling of all cases for 3 element coproduct" in {
      val handler: SIL => String = CoproductFold[SIL]
        .fold[String](identity)
        .fold[Int](_.toString)
        .fold[Long](_.toString)
      handler(Coproduct[SIL]("Mario")) must_== "Mario"
      handler(Coproduct[SIL](12)) must_== "12"
      handler(Coproduct[SIL](123L)) must_== "123"
    }

    "not allow adding handler for types that do not exist" in {
      val h = CoproductFold[SI]
      illTyped("h.handleTyped[Long](_ => 1)")
      h must not beNull
    }
    "not allow apply before all types are handled" in {
      val h = CoproductFold[SI]
      illTyped("h(sSI)")
      val h2 = h.fold[String](_ => 1)
      illTyped("h2(sSI)")
      val h3: SI => Int = h2.fold[Int](_ => 2)
      h3(sSI) must_== 1
    }
  }
}
