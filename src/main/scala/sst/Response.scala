package sst

import scala.language.existentials
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless._
import shapeless.syntax.typeable._
import shapeless.ops.coproduct.Inject

/** Parses AnyOf[Receive[X], Y, Z] structures into a coproduct (X +:+ Y +:+ Z) in Response[T].Out. */
@implicitNotFound("Not a response: ${A}")
sealed trait Response[A <: Action] {
  type Out <: Coproduct
  def parse(value: Any): Option[Out] = parts.view.flatMap(_.parser(value)).headOption
  def description: String = parts.map(_.tag.toString).mkString(" :+: ")
  protected type Parser = Any => Option[Out]
  protected case class Part(parser: Parser, tag: ClassTag[_])
  protected[sst] def parts: List[Part]
  protected def part[X: Typeable : ClassTag](implicit i: Inject[Out, X]) = {
    val p = (value: Any) => value.cast[X] map (Coproduct[Out](_))
    Part(p, implicitly[ClassTag[X]])
  }
}
sealed trait LowPriorityResponse {
  @implicitNotFound("Not a response: ${A}")
  type Aux[A <: Action, Out0 <: Coproduct] = Response[A] {type Out = Out0}

  implicit def anyOfRight[A: ClassTag : Typeable, B <: Action](implicit br: Response[B]) = new Response[AnyOf[B, Receive[A]]] {
    type Out = A :+: br.Out
    val parts: List[Part] = br.parts.map { part =>
      Part(in => part.parser(in).map(Inr(_)), part.tag)
    } :+ part[A]
  }
}
object Response extends LowPriorityResponse {
  def apply[A <: Action](implicit r: Response[A]): Aux[A, r.Out] = r

  implicit def receive[A: ClassTag : Typeable] = new Response[Receive[A]] {
    type Out = A :+: CNil
    val parts = part[A] :: Nil
  }
  implicit def anyOfLeft[A: ClassTag : Typeable, B <: Action](implicit br: Response[B]) = new Response[AnyOf[Receive[A], B]] {
    type Out = A :+: br.Out
    val parts: List[Part] = part[A] :: br.parts.map { part =>
      Part(in => part.parser(in).map(Inr(_)), part.tag)
    }
  }
}
