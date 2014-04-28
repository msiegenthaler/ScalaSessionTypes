package sst

sealed trait Action {
  type Type <: Action
  def description: Option[String]
  def describe(desc: String): Type
}
object Action extends ActionFactory

/** Send a message of type Value. */
case class Send[Value](description: Option[String] = None) extends Action {
  type Type = Send[Value]
  def describe(desc: String) = copy(description = Some(desc))
}
/** Receive a message of type Value. */
case class Receive[Value](description: Option[String] = None) extends Action {
  type Type = Receive[Value]
  def describe(desc: String) = copy(description = Some(desc))
}
/** Internal Choice. */
case class Choice[A <: Action, B <: Action](a: A, b: B, description: Option[String] = None) extends Action {
  type Type = Choice[A, B]
  def describe(desc: String) = copy(description = Some(desc))
}
/** External Choice. */
case class AnyOf[A <: Action, B <: Action](a: A, b: B, description: Option[String] = None) extends Action {
  type Type = AnyOf[A, B]
  def describe(desc: String) = copy(description = Some(desc))
}
/** Sequence: A then B. */
case class Then[A <: Action, Next <: Action](action: A, next: Next, description: Option[String] = None) extends Action {
  type Type = Then[A, Next]
  def describe(desc: String) = copy(description = Some(desc))
}
/** Performs A until Break is encountered. */
case class Repeat[A <: Action](a: Action, description: Option[String] = None) extends Action {
  type Type = Repeat[A]
  def describe(desc: String) = copy(description = Some(desc))
}
/** Exits the parent loop. */
case class Break(description: Option[String] = None) extends Action {
  type Type = Break
  def describe(desc: String) = copy(description = Some(desc))
}


trait ActionFactory {
  def apply[A <: Action](implicit f: InstanceFactory[A]): A = f()

  def send[Value]: Send[Value] = Send()
  def answer[Value] = send[Value]
  def receive[Value]: Receive[Value] = Receive()
  def anyOf[A <: Action, B <: Action](a: A, b: B) = AnyOf(a, b)
  def anyOf[A <: Action, B <: Action, C <: Action](a: A, b: B, c: C) = AnyOf(a, AnyOf(b, c))
  def anyOf[A <: Action, B <: Action, C <: Action, D <: Action](a: A, b: B, c: C, d: D) =
    AnyOf(a, AnyOf(b, AnyOf(c, d)))
  def choose[A <: Action, B <: Action](a: A, b: B) = Choice(a, b)
  def choose[A <: Action, B <: Action, C <: Action](a: A, b: B, c: C) = Choice(a, Choice(b, c))
  def choose[A <: Action, B <: Action, C <: Action, D <: Action](a: A, b: B, c: C, d: D) =
    Choice(a, Choice(b, Choice(c, d)))
  def repeat[A <: Action](a: Action) = Repeat(a)
  def break = Break()
}

class ActionOps[Self <: Action](action: Self) {
  def asTree(implicit d: Description.TS[Self]) = Description.asTree[Self]

  def send[Value] = andThen(Send[Value]())
  def answer[Value] = send[Value]
  def answerEither[A, B] = answerEither2[A, B]
  def answerEither2[A, B] = chooseFrom(Action.send[A], Action.send[B])
  def answerEither3[A, B, C] = chooseFrom(Action.send[A], Action.send[B], Action.send[C])
  def answerEither4[A, B, C, D] = chooseFrom(Action.send[A], Action.send[B], Action.send[C], Action.send[D])

  def receive[Value] = andThen(Receive[Value]())
  def receiveAnyOf[A, B] = receiveAnyOf2[A, B]
  def receiveAnyOf2[A, B] = anyOf(Action.receive[A], Action.receive[B])
  def receiveAnyOf3[A, B, C] = anyOf(Action.receive[A], Action.receive[B], Action.receive[C])
  def receiveAnyOf4[A, B, C, D] = anyOf(Action.receive[A], Action.receive[B], Action.receive[C], Action.receive[D])

  def anyOf[A <: Action](a: A) = andThen(a)
  def anyOf[A <: Action, B <: Action](a: A, b: B) = andThen(a :&: b)
  def anyOf[A <: Action, B <: Action, C <: Action](a: A, b: B, c: C) = andThen(a :&: b :&: c)
  def anyOf[A <: Action, B <: Action, C <: Action, D <: Action](a: A, b: B, c: C, d: D) = andThen(a :&: b :&: c :&: d)
  def chooseFrom[A <: Action](a: A) = andThen(a)
  def chooseFrom[A <: Action, B <: Action](a: A, b: B) = andThen(a :@: b)
  def chooseFrom[A <: Action, B <: Action, C <: Action](a: A, b: B, c: C) = andThen(a :@: b :@: c)
  def chooseFrom[A <: Action, B <: Action, C <: Action, D <: Action](a: A, b: B, c: C, d: D) = andThen(a :@: b :@: c :@: d)

  def andThen[A <: Action](a: A): Then[Self, A] = Then(action, a)
  def :&:[A <: Action](a: A): AnyOf[A, Self] = AnyOf(a, action)
  def :@:[A <: Action](a: A): Choice[A, Self] = Choice(a, action)
  def repeat: Repeat[Self] = Repeat(action)
  def repeat[A <: Action](action: A) = andThen(Repeat(action))
  def break = andThen(Break())

  def <|(desc: String): Self = action.describe(desc).asInstanceOf[Self]
}

