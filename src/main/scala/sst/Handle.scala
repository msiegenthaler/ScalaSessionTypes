package sst

import shapeless._
import scala.annotation.implicitNotFound

object Handle {
  type I = Int :+: CNil
  type SI = String :+: Int :+: CNil
  val a = Coproduct[SI]("hi")


  @implicitNotFound("${From} does not contain ${What}")
  trait Remove[From <: Coproduct, What] extends DepFn2[From, What] {
    type Out <: Coproduct
  }
  trait LowPrioRemove {
    type Aux[From <: Coproduct, What, Res <: Coproduct] = Remove[From, What] {type Out = Res}
    implicit def notFound[H, T <: Coproduct, What]: Aux[H :+: T, What, H :+: T] = new Remove[H :+: T, What] {
      type Out = H :+: T
      def apply(from: H :+: T, what: What) = from
    }
  }
  object Remove extends LowPrioRemove {
    def apply[From <: Coproduct, What](implicit w: Remove[From, What]): Remove[From, What] {type Out = w.Out} = w

    implicit def found[H, T <: Coproduct, What](implicit a: What =:= H): Aux[H :+: T, What, T] = new Remove[H :+: T, What] {
      type Out = T
      def apply(from: H :+: T, what: What) = ???
    }
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


  trait Handler[On <: Coproduct, Remaining <: Coproduct, -R]
  @implicitNotFound("Cannot run handler, unhandled cases left: ${R}")
  implicit class ExecutableHandler[On <: Coproduct, R](handler: Handler[On, CNil, R]) {
    def run(on: On): R = ??? // TODO
  }

  object Handler {
    def apply[On <: Coproduct](on: On): Handler[On, On, Nothing] = ???
    def apply[On <: Coproduct, Remaining <: Coproduct, B1, A, B >: B1](handler: Handler[On, Remaining, B1], f: A => B)
                                                                      (implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A]): Handler[On, r.Out, B] = {
      ???
    }
  }

  //  val a: String :+: Int :+: CNil = ???
  val x: SI = ???
  val h1 = Handler(x)
  val h2 = Handler(h1, (_: String) => 12)
  val h3 = Handler(h2, (_: Int) => 12)
  //  val h4 = Handler(h3, (_: String) => 12)
  //  val h5 = Handler[SI, SI, Exception, Int](h1, (_: Exception) => 12)
  h3.run(x)


}
