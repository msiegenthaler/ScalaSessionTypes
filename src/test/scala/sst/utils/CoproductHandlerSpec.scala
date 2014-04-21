package sst.utils

import scala.language.reflectiveCalls
import org.specs2.mutable._
import shapeless._
import shapeless.test.illTyped

class CoproductHandlerSpec extends Specification {
  type SI = String :+: Int :+: CNil
  type SIL = String :+: Int :+: Long :+: CNil
  val sSI = Coproduct[SI]("Mario")

  "CoproductHandler" should {
    "allow typesafe handling of all cases" in {
      val handler = CoproductHandler[SIL]
        .handleTyped[String](identity)
        .handleTyped[Int](_.toString)
        .handleTyped[Long](_.toString)
      CoproductHandler.run(handler, Coproduct[SIL]("Mario")) must_== "Mario"
      CoproductHandler.run(handler, Coproduct[SIL](12)) must_== "12"
      CoproductHandler.run(handler, Coproduct[SIL](123L)) must_== "123"
    }

    "not allow adding handler for types that do not exist" in {
      val h = CoproductHandler[SI]
      illTyped("h.handleTyped[Long](_ => 1)")
      h must not beNull
    }
    "not return a handler before all types are handled" in {
      val h = CoproductHandler[SI]
      illTyped("CoproductHandler.run(h, sSI)")
      val h2 = h.handleTyped[String](_ => 1)
      illTyped("CoproductHandler.run(h, sSI)")
      val h3 = h2.handleTyped[Int](_ => 2)
      CoproductHandler.run(h3, sSI) must_== 1
    }
  }
}
