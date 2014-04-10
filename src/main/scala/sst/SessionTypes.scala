package sst

import scala.annotation.implicitNotFound

object SessionTypes {

  sealed trait Action

  sealed trait Send[Value] extends Action
  sealed trait Receive[Value] extends Action
  /** Internal Choice. */
  sealed trait Choice[A <: Action, B <: Action] extends Action
  /** External Choice. */
  sealed trait AnyOf[A <: Action, B <: Action] extends Action
  /** Sequence: A then B. */
  sealed trait Cons[A <: Action, B <: Action] extends Action

  type ![Value] = Send[Value]
  type ?[Value] = Receive[Value]
  type :>:[A <: Action, B <: Action] = Cons[A, B]
  type :&:[A <: Action, B <: Action] = AnyOf[A, B]
  type :@:[A <: Action, B <: Action] = Choice[A, B]

  private def witness = null

  @implicitNotFound("Protocol violation: ${A} and ${B} are not dual (client/server)")
  sealed trait Dual[A <: Action, B <: Action]
  implicit def dualSend[V]: Dual[Send[V], Receive[V]] = witness
  implicit def dualReceive[V]: Dual[Receive[V], Send[V]] = witness
  implicit def dualChoiceAnyOf[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Dual[A1, A2], wb: Dual[B1, B2]): Dual[Choice[A1, B1], AnyOf[A2, B2]] = witness
  implicit def dualChoiceAnyOfInvert[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Dual[A1, B2], wb: Dual[B1, A2]): Dual[Choice[A1, B1], AnyOf[A2, B2]] = witness
  implicit def dualAnyOfChoice[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Dual[A1, A2], wb: Dual[B1, B2]): Dual[AnyOf[A1, B1], Choice[A2, B2]] = witness
  implicit def dualAnyOfChoiceInvert[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Dual[A1, B2], wb: Dual[B1, A2]): Dual[AnyOf[A1, B1], Choice[A2, B2]] = witness
  implicit def dualCons[A1 <: Action, A2 <: Action, B1 <: Action, B2 <: Action](implicit wa: Dual[A1, A2], wb: Dual[B1, B2]): Dual[Cons[A1, B1], Cons[A2, B2]] = witness
  def dual[A <: Action, B <: Action](implicit w: Dual[A, B]) = witness
}
