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
}

object Opposite {
  def is[A <: Action, B <: Action](implicit w: Aux[A, B]) = ()

  def apply[A <: Action](implicit R: Opposite[A]): Opposite[A] {type Out = R.Out} = R

  @implicitNotFound("Not dual (client/server): ${A} and ${Out0}")
  type Aux[A <: Action, Out0 <: Action] = Opposite[A] {type Out = Out0}

  implicit def sendOpposite[A]: Aux[Send[A], Receive[A]] = witness
  implicit def receiveOpposite[A]: Aux[Receive[A], Send[A]] = witness
  implicit def thenOpposite[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit a: Aux[A1, A2], b: Aux[B1, B2]): Aux[Then[A1, B1], Then[A2, B2]] = witness
  implicit def choiceOpposite[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Aux[A1, A2], wb: Aux[B1, B2]): Aux[Choice[A1, B1], AnyOf[A2, B2]] = witness
  implicit def anyOfOpposite[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Aux[A1, A2], wb: Aux[B1, B2]): Aux[AnyOf[A1, B1], Choice[A2, B2]] = witness
  implicit def dualRepeat[A1 <: Action, A2 <: Action](implicit w: Aux[A1, A2]): Aux[Repeat[A1], Repeat[A2]] = witness
  implicit def dualBreak: Aux[Break, Break] = witness
}
