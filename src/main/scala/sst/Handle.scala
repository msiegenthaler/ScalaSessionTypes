package sst

import shapeless._
import scala.annotation.implicitNotFound

object Handle {
  type I = Int :+: CNil
  type SI = String :+: Int :+: CNil
  val a = Coproduct[SI]("hi")


  @implicitNotFound("${From} does not contain ${What}")
  trait Remove[From <: Coproduct, What] {
    type Out <: Coproduct
  }
  trait LowPrioRemove {
    type Aux[From <: Coproduct, What, Res <: Coproduct] = Remove[From, What] {type Out = Res}
    implicit def notFound[H, T <: Coproduct, What]: Aux[H :+: T, What, H :+: T] = witness
  }
  object Remove extends LowPrioRemove {
    def apply[From <: Coproduct, What](implicit w: Remove[From, What]): Remove[From, What] {type Out = w.Out} = w

    implicit def found[H, T <: Coproduct, What](implicit a: What =:= H): Aux[H :+: T, What, T] = witness
  }

  {
    val r = Remove[SI, String]
    implicitly[I =:= r.Out]
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
    //    implicit def notCurrent[H, T <: Coproduct, What]: Aux[H :+: T, What, True] = witness
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


  trait Handler[On <: Coproduct, Remaining <: Coproduct, R] {
    def run()(implicit w: HandlerIsRunnable[Remaining]): R
    def handle[A, B](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A]) = Handler(this, f)
    def handleTyped[A] = new AnyRef {
      def apply[B](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A]) = handle(f)
    }
  }
  object Handler {
    def apply[On <: Coproduct](on: On): Handler[On, On, Nothing] = ???
    def apply[On <: Coproduct, Remaining <: Coproduct, B1, A, B >: B1](handler: Handler[On, Remaining, B1], f: A => B)
                                                                      (implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A]): Handler[On, r.Out, B] = {
      ???
    }
  }
  @implicitNotFound("Cannot run handler, unhandled cases left: ${Remaining}")
  sealed trait HandlerIsRunnable[Remaining <: Coproduct]
  object HandlerIsRunnable {
    implicit def emptyIsRunnable: HandlerIsRunnable[CNil] = witness
  }

  {
    val x: SI = ???
    val r = Handler(x).
      handleTyped[String](_.length).
      handle((a: Int) => a).
      //      handle((a: Exception) => a).
      run()
  }

}
