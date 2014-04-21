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
  /** Remove a type from a Coproduct (typelevel only). */
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


  /** Check if a type is contained in a Coproduct (typelevel only). */
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


  /** Concat two Coproducts (typelevel only). */
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
}
