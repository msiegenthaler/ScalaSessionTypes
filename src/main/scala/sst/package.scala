import scala.language.implicitConversions

package object sst extends ActionFactory {
  implicit def actionOps[A <: Action](a: A): ActionOps[A] = new ActionOps(a)
  implicit class ActionDescription(description: String) {
    def |>[A <: Action](action: A) = action.describe(description)
  }

  type ![Value] = Send[Value]
  type ?[Value] = Receive[Value]
  type :>:[A <: Action, B <: Action] = Then[A, B]
  type :&:[A <: Action, B <: Action] = AnyOf[A, B]
  type :@:[A <: Action, B <: Action] = Choice[A, B]
  type :| = Break
}