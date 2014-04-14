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

  /** Performs A until Break is encoutered. */
  sealed trait Repeat[A <: Action] extends Action
  /** Exits the parent loop. */
  sealed trait Break extends Action

  type ![Value] = Send[Value]
  type ?[Value] = Receive[Value]
  type :>:[A <: Action, B <: Action] = Cons[A, B]
  type :&:[A <: Action, B <: Action] = AnyOf[A, B]
  type :@:[A <: Action, B <: Action] = Choice[A, B]
  type :| = Break

  def witness[A]: A = null.asInstanceOf[A]
}
