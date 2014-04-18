package sst.utils

import org.specs2.mutable._
import shapeless._
import shapeless.test._
import CoproductOps._

class CoproductOpsSpec extends Specification {
  private def assertCompiled = true must beTrue

  type I = Int :+: CNil
  type S = String :+: CNil
  type L = Long :+: CNil
  type SI = String :+: Int :+: CNil
  type SL = String :+: Long :+: CNil
  type IL = Int :+: Long :+: CNil
  type SIL = String :+: Int :+: Long :+: CNil

  "Remove" should {
    "be I for [SI, String]" in {
      val r = Remove[SI, String]
      implicitly[I =:= r.Out]
      assertCompiled
    }
    "be S for [SI, Int]" in {
      val r = Remove[SI, Int]
      implicitly[S =:= r.Out]
      assertCompiled
    }
    "be SI for [SIL, Long" in {
      val r = Remove[SIL, Long]
      implicitly[SI =:= r.Out]
      assertCompiled
    }
    "be SL for [SIL, Int]" in {
      val r = Remove[SIL, Int]
      implicitly[SL =:= r.Out]
      assertCompiled
    }
    "be IL for [SIL, S]" in {
      val r = Remove[SIL, String]
      implicitly[IL =:= r.Out]
      assertCompiled
    }

    "not compile for [I, S]" in {
      illTyped("Remove[L,S]")
      assertCompiled
    }
    "not compile for [IL, S]" in {
      illTyped("Remove[IL,S]")
      assertCompiled
    }
  }

  "Contains" should {
    "be True for [SI, String]" in {
      val r = Contains[SI, String]
      implicitly[True =:= r.Out]
      assertCompiled
    }
    "be True in [SI, Int]" in {
      val r = Contains[SI, Int]
      implicitly[True =:= r.Out]
      assertCompiled
    }
    "be False in [SI, Long" in {
      val r = Contains[SI, Long]
      implicitly[False =:= r.Out]
      assertCompiled
    }
  }

  "Concat" should {
    "be I for [I, CNil]" in {
      val r = Concat[I, CNil]
      implicitly[I =:= r.Out]
      assertCompiled
    }
    "be SI for [SI, CNil]" in {
      val r = Concat[SI, CNil]
      implicitly[SI =:= r.Out]
      assertCompiled
    }
    "be I for [CNil and I]" in {
      val r = Concat[CNil, I]
      implicitly[I =:= r.Out]
      assertCompiled
    }
    "be IL for [I and L]" in {
      val r = Concat[I, L]
      implicitly[IL =:= r.Out]
      assertCompiled
    }
    "be SIL for [SI, L]" in {
      val r = Concat[SI, L]
      implicitly[SIL =:= r.Out]
      assertCompiled
    }
    "be SIL for [S, IL]" in {
      val r = Concat[S, IL]
      implicitly[SIL =:= r.Out]
      assertCompiled
    }
    "be Double :+: SIL for [Double :+: S, IL]" in {
      val r = Concat[Double :+: S, IL]
      implicitly[(Double :+: SIL) =:= r.Out]
      assertCompiled
    }
  }
}