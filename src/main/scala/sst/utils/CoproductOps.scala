package sst.utils

import shapeless.{Coproduct, CNil, :+:}
import scala.annotation.implicitNotFound
import sst._

/**
 * Operations on Coproducts:
 * - Remove (type-level only)
 * - Contains (type-level)
 */
object CoproductOps {
  type I = Int :+: CNil
  type S = String :+: CNil
  type L = Long :+: CNil
  type SI = String :+: Int :+: CNil
  type SL = String :+: Long :+: CNil
  type IL = Int :+: Long :+: CNil
  type SIL = String :+: Int :+: Long :+: CNil
  val a = Coproduct[SI]("hi")


  @implicitNotFound("${From} does not contain ${What}")
  trait Remove[From <: Coproduct, What] {
    type Out <: Coproduct
  }
  trait LowPrioRemove {
    type Aux[From <: Coproduct, What, Res <: Coproduct] = Remove[From, What] {type Out = Res}
    implicit def notFound[H, T <: Coproduct, What](implicit w: Remove[T, What]): Aux[H :+: T, What, H :+: w.Out] = witness
  }
  object Remove extends LowPrioRemove {
    def apply[From <: Coproduct, What](implicit w: Remove[From, What]): Remove[From, What] {type Out = w.Out} = w

    implicit def found[H, T <: Coproduct, What](implicit a: What =:= H): Aux[H :+: T, What, T] = witness
  }

  sealed trait Bool
  sealed trait True extends Bool
  sealed trait False extends Bool
  sealed trait Maybe extends Bool

  trait Contains[In <: Coproduct, What] {
    type Out <: Bool
  }
  trait LowPrioContains {
    type Aux[In <: Coproduct, What, Res <: Bool] = Contains[In, What] {type Out = Res}
    implicit def notCurrent[H, T <: Coproduct, What](implicit t: Contains[T, What]): Aux[H :+: T, What, t.Out] = witness
  }
  object Contains extends LowPrioContains {
    def apply[In <: Coproduct, What](implicit c: Contains[In, What]): Contains[In, What] {type Out = c.Out} = witness

    @implicitNotFound("${In} does not contain ${What}")
    type Yes[In <: Coproduct, What] = Contains.Aux[In, What, True]
    @implicitNotFound("${In} already contains ${What}")
    type No[In <: Coproduct, What] = Contains.Aux[In, What, False]

    implicit def found[H, T <: Coproduct, What](implicit a: What =:= H, t: Contains[T, What]): Aux[H :+: T, What, True] = witness
    implicit def end[What]: Aux[CNil, What, False] = witness
  }

  @implicitNotFound("Cannot concat ${A} and ${B}")
  trait Concat[A <: Coproduct, B <: Coproduct] {
    type Out <: Coproduct
  }
  object Concat {
    def apply[A <: Coproduct, B <: Coproduct](implicit c: Concat[A, B]): Concat[A, B] {type Out = c.Out} = witness
    type Aux[A <: Coproduct, B <: Coproduct, Res <: Coproduct] = Concat[A, B] {type Out = Res}

    implicit def concatCNil[A <: Coproduct]: Aux[CNil, A, A] = witness
    implicit def concatCons[H, T <: Coproduct, R <: Coproduct](implicit nr: Concat[T, R]): Aux[H :+: T, R, H :+: nr.Out] = witness
  }

  //Tests for Concat
  {
    val r1 = Concat[I, CNil]
    implicitly[I =:= r1.Out]

    val r2 = Concat[SI, CNil]
    implicitly[SI =:= r2.Out]

    val r3 = Concat[CNil, I]
    implicitly[I =:= r3.Out]

    val r4 = Concat[I, L]
    implicitly[IL =:= r4.Out]

    val r5 = Concat[SI, L]
    implicitly[SIL =:= r5.Out]

    val r6 = Concat[S, IL]
    implicitly[SIL =:= r6.Out]

    val r7 = Concat[Double :+: S, IL]
    implicitly[(Double :+: SIL) =:= r7.Out]
  }

  //Tests for Remove
  {
    val r = Remove[SI, String]
    implicitly[I =:= r.Out]

    val r2 = Remove[SI, Int]
    implicitly[S =:= r2.Out]

    val r3 = Remove[SIL, Long]
    implicitly[SI =:= r3.Out]

    val r4 = Remove[SIL, Int]
    implicitly[SL =:= r4.Out]

    val r5 = Remove[SIL, String]
    implicitly[IL =:= r5.Out]
  }
  //Tests for Contains
  {
    val r1 = Contains[SI, String]
    implicitly[True =:= r1.Out]
    val r2 = Contains[SI, Int]
    implicitly[True =:= r2.Out]
    val r3 = Contains[SI, Long]
    implicitly[False =:= r3.Out]
  }

}
