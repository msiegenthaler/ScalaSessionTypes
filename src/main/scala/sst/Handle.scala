package sst

import scala.language.reflectiveCalls
import scala.annotation.implicitNotFound
import shapeless._
import shapeless.ops.coproduct._
import sst.CoproductOps._

object Handle {
  sealed class Handler[On <: Coproduct, Remaining <: Coproduct, R](protected val fun: PartialFunction[On, R]) {
    def handler: On => R = on =>
      fun.lift(on).getOrElse(throw new MatchError(s"Handler did not match $on"))
    def handle[A, B >: R](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[On, A]) =
      Handler.compose(this, f)
    def handleTyped[A] = new AnyRef {
      def apply[B >: R](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[On, A]) = handle(f)
    }
  }
  object Handler {
    def apply[On <: Coproduct]: Handler[On, On, Nothing] = new Handler(PartialFunction.empty)
    private def compose[On <: Coproduct, Remaining <: Coproduct, B1, A, B >: B1]
      (handler: Handler[On, Remaining, B1], f: A => B)
      (implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[On, A]): Handler[On, r.Out, B] = {

      def fun = new PartialFunction[On, B] {
        def isDefinedAt(x: On) = s(x).isDefined
        def apply(x: On) = f(s(x).get)
      }
      new Handler(handler.fun orElse fun)
    }
  }
  @implicitNotFound("Cannot run handler, unhandled cases left: ${Remaining}")
  sealed trait HandlerIsRunnable[Remaining <: Coproduct]
  object HandlerIsRunnable {
    implicit def emptyIsRunnable: HandlerIsRunnable[CNil] = witness
  }
}
