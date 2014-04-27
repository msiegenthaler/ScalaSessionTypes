package sst.utils

import scala.annotation.implicitNotFound
import shapeless._
import shapeless.ops.coproduct._
import sst.utils.CoproductOps._
import sst.utils.CoproductOps.Contains.Yes

/**
 * Handle each type in a Coproduct with a separate handler and then provide a combined function. The
 * fold function is only available if all types are handled (compile time checked). It is also checked at
 * the compile time that all handled types are actually possible.
 * Example (will return a function:
 * <code>
 * CoproductFold[Int :+: String :+: CNil].fold[String](_ => 1).fold[Int](_ => 2)(value)
 * CoproductFold[Int :+: String :+: CNil].fold[String](_ => 1).fold[Int](_ => 2).fun
 * </code>
 * Examples that do not compile:
 * <code>
 * CoproductFold[Int :+: String :+: CNil].fold[String].fun
 * CoproductFold[Int :+: String :+: CNil].fold[String](_ => 1).fold[Int](_ => 2).fold[Long](_ => 3).fun
 * </code>
 */
sealed class CoproductFold[On <: Coproduct, Remaining <: Coproduct, R](protected val partialFun: PartialFunction[On, R]) {
  def apply(on: On)(implicit w: CoproductFoldIsComplete[Remaining]): R = fun(implicitly)(on)

  /** Use if a function reference is needed. */
  def fun(implicit w: CoproductFoldIsComplete[Remaining]): On => R = on =>
    partialFun.lift(on).getOrElse(throw new MatchError(s"Handler did not match $on"))


  def fold[A]: CoproductFoldFun[A, On, Remaining, R] = new CoproductFoldFun[A, On, Remaining, R] {
    def apply[B >: R](f: (A) => B)(implicit r: Remove[Remaining, A], contains: Yes[Remaining, A], s: Selector[On, A]) = {
      def f2 = new PartialFunction[On, B] {
        def isDefinedAt(x: On) = s(x).isDefined
        def apply(x: On) = f(s(x).get)
      }
      new CoproductFold(partialFun orElse f2)
    }
  }
}
object CoproductFold {
  def apply[On <: Coproduct]: CoproductFold[On, On, Nothing] = new CoproductFold(PartialFunction.empty)

  private def compose[On <: Coproduct, Remaining <: Coproduct, B1, A, B >: B1]
  (handler: CoproductFold[On, Remaining, B1], f: A => B)
    (implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[On, A]): CoproductFold[On, r.Out, B] = {

    def fun = new PartialFunction[On, B] {
      def isDefinedAt(x: On) = s(x).isDefined
      def apply(x: On) = f(s(x).get)
    }
    new CoproductFold(handler.partialFun orElse fun)
  }
}

@implicitNotFound("Fold incomplete: ${Remaining} are unhandled")
sealed trait CoproductFoldIsComplete[Remaining <: Coproduct]
object CoproductFoldIsComplete {
  implicit def emptyIsComplete: CoproductFoldIsComplete[CNil] = witness
}

sealed trait CoproductFoldFun[A, On2 <: Coproduct, Remaining2 <: Coproduct, R] {
  def apply[B2 >: R](f: A => B2)
    (implicit r: Remove[Remaining2, A], contains: Contains.Yes[Remaining2, A], s: Selector[On2, A]): CoproductFold[On2, r.Out, B2]
}
