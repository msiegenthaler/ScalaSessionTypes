package sst

import shapeless._
import scala.annotation.implicitNotFound
import sst.utils.witness

/**
 * Dual between producer and consumer (client/server).
 * <p>
 * Opposite.is[A,B] checks if A is the counterpart of B.
 * Usage:
 * Opposite.is[Client, Server] //only compiles if Client and Server are dual
 * <p>
 * Opposite[A] calulates the counterpart (client for server or vice-versa) for A.
 * Usage:
 * val server = Opposite[Client]
 * type Server = server.Out
 * Opposite.is[Client, Server]
 */
@implicitNotFound("No opposite: ${A}")
trait Opposite[A <: Action] extends DepFn1[A] {
  type Out <: Action
  def apply(): Out
}

object Opposite {
  def is[A <: Action, B <: Action](implicit w: Aux[A, B]) = ()

  def apply[A <: Action](implicit r: Opposite[A]): r.Out = r()

  @implicitNotFound("Not dual (client/server): ${A} and ${Out0}")
  type Aux[A <: Action, Out0 <: Action] = Opposite[A] {type Out = Out0}

  implicit def sendOpposite[A](implicit i: InstanceFactory[Receive[A]]): Aux[Send[A], Receive[A]] = make
  implicit def receiveOpposite[A](implicit i: InstanceFactory[Send[A]]): Aux[Receive[A], Send[A]] = make
  implicit def thenOpposite[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit a: Aux[A1, A2], b: Aux[B1, B2], i: InstanceFactory[Then[A2, B2]]): Aux[Then[A1, B1], Then[A2, B2]] = make
  implicit def choiceOpposite[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Aux[A1, A2], wb: Aux[B1, B2], i: InstanceFactory[AnyOf[A2, B2]]): Aux[Choice[A1, B1], AnyOf[A2, B2]] = make
  implicit def anyOfOpposite[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Aux[A1, A2], wb: Aux[B1, B2], i: InstanceFactory[Choice[A2, B2]]): Aux[AnyOf[A1, B1], Choice[A2, B2]] = make
  implicit def dualRepeat[A1 <: Action, A2 <: Action](implicit w: Aux[A1, A2], i: InstanceFactory[Repeat[A2]]): Aux[Repeat[A1], Repeat[A2]] = make
  implicit def dualBreak(implicit i: InstanceFactory[Break]): Aux[Break, Break] = make

  private def make[A <: Action, O <: Action](implicit i: InstanceFactory[O]): Aux[A, O] = new Opposite[A] {
    override type Out = O
    override def apply() = i()
    override def apply(v: A) = apply()
  }
}
