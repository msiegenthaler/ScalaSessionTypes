package sst.utils

import scala.annotation.implicitNotFound
import shapeless._
import shapeless.ops.coproduct._
import sst.utils.CoproductOps._
import sst.utils.CoproductOps.Contains.Yes

/**
 * Handle each type in a Coproduct with a separate handler and then provide a combined handler function. The
 * handler function is only available if all types are handled (compile time checked). It is also checked at
 * the compile time that all handled types are actually possible.
 * Example (will return a function:
 * <code>
 * CoproductMapper[Int :+: String :+: CNil].map[String](_ => 1).map[Int](_ => 2)(value)
 * CoproductMapper[Int :+: String :+: CNil].map[String](_ => 1).map[Int](_ => 2).fun
 * </code>
 * Examples that do not compile:
 * <code>
 * CoproductMapper[Int :+: String :+: CNil].map[String].fun
 * CoproductMapper[Int :+: String :+: CNil].map[String](_ => 1).map[Int](_ => 2).map[Long](_ => 3).fun
 * </code>
 */
sealed class CoproductMapper[On <: Coproduct, Remaining <: Coproduct, R](protected val partialFun: PartialFunction[On, R]) {
  def apply(on: On)(implicit w: CoproductMapperIsComplete[Remaining]): R = fun(implicitly)(on)

  /** Use if a function reference is needed. */
  def fun(implicit w: CoproductMapperIsComplete[Remaining]): On => R = on =>
    partialFun.lift(on).getOrElse(throw new MatchError(s"Handler did not match $on"))


  def map[A]: CoproductMapperFun[A, On, Remaining, R] = new CoproductMapperFun[A, On, Remaining, R] {
    def apply[B >: R](f: (A) => B)(implicit r: Remove[Remaining, A], contains: Yes[Remaining, A], s: Selector[On, A]) = {
      def f2 = new PartialFunction[On, B] {
        def isDefinedAt(x: On) = s(x).isDefined
        def apply(x: On) = f(s(x).get)
      }
      new CoproductMapper(partialFun orElse f2)
    }
  }
}
object CoproductMapper {
  def apply[On <: Coproduct]: CoproductMapper[On, On, Nothing] = new CoproductMapper(PartialFunction.empty)

  private def compose[On <: Coproduct, Remaining <: Coproduct, B1, A, B >: B1]
  (handler: CoproductMapper[On, Remaining, B1], f: A => B)
    (implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[On, A]): CoproductMapper[On, r.Out, B] = {

    def fun = new PartialFunction[On, B] {
      def isDefinedAt(x: On) = s(x).isDefined
      def apply(x: On) = f(s(x).get)
    }
    new CoproductMapper(handler.partialFun orElse fun)
  }
}

@implicitNotFound("Cannot mapper incomplete: ${Remaining} are unhandled")
sealed trait CoproductMapperIsComplete[Remaining <: Coproduct]
object CoproductMapperIsComplete {
  implicit def emptyIsComplete: CoproductMapperIsComplete[CNil] = witness
}

sealed trait CoproductMapperFun[A, On2 <: Coproduct, Remaining2 <: Coproduct, R] {
  def apply[B2 >: R](f: A => B2)
    (implicit r: Remove[Remaining2, A], contains: Contains.Yes[Remaining2, A], s: Selector[On2, A]): CoproductMapper[On2, r.Out, B2]
}
