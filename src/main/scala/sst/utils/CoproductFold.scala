package sst.utils

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import shapeless._
import shapeless.ops.coproduct._
import sst.utils.CoproductOps._
import sst.utils.CoproductOps.Contains.Yes


/**
 * Handle each type in a Coproduct with a separate handler and then provide a combined function. The
 * fold function is only available if all types are handled (compile time checked). It is also checked at
 * the compile time that all handled types are actually possible.
 * Example:
 * <code>
 * CoproductFold[Int :+: String :+: CNil].fold[String](_ => 1).fold[Int](_ => 2)(value)
 * </code>
 * Examples that do not compile:
 * <code>
 * CoproductFold[Int :+: String :+: CNil].fold[String]
 * CoproductFold[Int :+: String :+: CNil].fold[String](_ => 1).fold[Int](_ => 2).fold[Long](_ => 3)
 * </code>
 */
sealed trait CoproductFold[On <: Coproduct, Remaining <: Coproduct] {
  type Result
  protected[utils] def foldingFun: PartialFunction[On, Result]
}
object CoproductFold extends CoproductFoldLowPriority {
  def apply[On <: Coproduct]: Aux[On, On, Nothing] = new CoproductFold[On, On] {
    type Result = Nothing
    val foldingFun = PartialFunction.empty
  }

  type AuxLast[O <: Coproduct, Rem <: Coproduct, R] = CoproductFoldLast[O, Rem] {type Result = R}
  implicit def complete[On <: Coproduct, Last](c: CoproductFold[On, Last :+: CNil]): AuxLast[On, Last :+: CNil, c.Result] = {
    new CoproductFoldLast[On, Last :+: CNil] {
      type Result = c.Result
      def foldingFun = c.foldingFun
    }
  }
}
trait CoproductFoldLowPriority {
  type Aux[O <: Coproduct, Rem <: Coproduct, R] = CoproductFold[O, Rem] {type Result = R}
  type AuxMore[O <: Coproduct, Rem <: Coproduct, R] = CoproductFoldMore[O, Rem] {type Result = R}

  implicit def incomplete[On <: Coproduct, Remaining <: Coproduct](c: CoproductFold[On, Remaining]): AuxMore[On, Remaining, c.Result] = {
    new CoproductFoldMore[On, Remaining] {
      type Result = c.Result
      def foldingFun = c.foldingFun
    }
  }

  sealed trait CoproductFoldMore[On <: Coproduct, Remaining <: Coproduct] {
    type Result
    protected def foldingFun: PartialFunction[On, Result]

    def fold[A] = new Fun[A]
    final class Fun[A] {
      def apply[B >: Result](f: A => B)(implicit r: Remove[Remaining, A], contains: Yes[Remaining, A], s: Selector[On, A]): Aux[On, r.Out, B] = {
        val f2 = new PartialFunction[On, B] {
          def isDefinedAt(x: On) = s(x).isDefined
          def apply(x: On) = f(s(x).get)
        }
        val fun = foldingFun orElse f2
        new CoproductFold[On, r.Out] {
          type Result = B
          val foldingFun = fun
        }
      }
    }
  }
  sealed trait CoproductFoldLast[On <: Coproduct, Remaining <: Coproduct] {
    type Result
    protected def foldingFun: PartialFunction[On, Result]

    def fold[A] = new Fun[A]
    final class Fun[A] {
      def apply[B >: Result](f: A => B)(implicit contains: Yes[Remaining, A], s: Selector[On, A]): On => B = {
        val f2 = new PartialFunction[On, B] {
          def isDefinedAt(x: On) = s(x).isDefined
          def apply(x: On) = f(s(x).get)
        }
        val fun = foldingFun orElse f2
        on => fun.lift(on).getOrElse(throw new MatchError(s"Handler did not match $on"))
      }
    }
  }
}