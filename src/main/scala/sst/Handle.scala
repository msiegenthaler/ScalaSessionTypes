package sst

import scala.language.reflectiveCalls
import shapeless._
import scala.annotation.implicitNotFound
import shapeless.ops.coproduct._

object Handle {
  type I = Int :+: CNil
  type S = String :+: CNil
  type L = String :+: CNil
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

  ///

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

  {
    val r1 = Contains[SI, String]
    implicitly[True =:= r1.Out]
    val r2 = Contains[SI, Int]
    implicitly[True =:= r2.Out]
    val r3 = Contains[SI, Long]
    implicitly[False =:= r3.Out]
  }

  ///

  //


  final class Handler[On <: Coproduct, Remaining <: Coproduct, R](protected val fun: PartialFunction[On, R]) {
    def handler: On => R = on =>
      fun.lift(on).getOrElse(throw new MatchError(s"Handler did not match $on"))
    def handle[A, B](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[On, A]) =
      Handler.compose(this, f)
    def handleTyped[A] = new AnyRef {
      def apply[B](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[On, A]) = handle(f)
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

  {
    val x = Coproduct[SI]("Mario")
    val r = Handler[SI].
      handleTyped[String](_.length).
      handleTyped[Int]((a: Int) => a).
      handler(x)
  }
}
