package object sst {
  sealed trait Action
  case class Send[Value]() extends Action
  case class Receive[Value]() extends Action
  /** Internal Choice. */
  case class Choice[A <: Action, B <: Action](a: A, b: B) extends Action
  /** External Choice. */
  case class AnyOf[+A <: Action, +B <: Action](a: A, b: B) extends Action
  /** Sequence: A then B. */
  case class Then[A <: Action, Next <: Action](action: A, next: Next) extends Action
  /** Performs A until Break is encoutered. */
  case class Repeat[A <: Action](a: Action) extends Action
  /** Exits the parent loop. */
  case class Break() extends Action

  object Action {
    def send[Value]: Send[Value] = Send()
    def receive[Value]: Receive[Value] = Receive()
    def anyOf[A <: Action, B <: Action](a: A, b: B): AnyOf[A, B] = AnyOf(a, b)
    def choose[A <: Action, B <: Action](a: A, b: B): Choice[A, B] = Choice(a, b)
    def break = Break()
  }
  implicit class ActionOps[Self <: Action](action: Self) {
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
    def break = andThen(Break())
  }

  import Action._

  val x = send[Int].
    send[Long].receiveAnyOf2[String, Exception].answer[Unit].
    chooseFrom(
      send[Int].receive[String],
      send[String].receive[Int])


  type ![Value] = Send[Value]
  type ?[Value] = Receive[Value]
  type :>:[A <: Action, B <: Action] = Then[A, B]
  type :&:[A <: Action, B <: Action] = AnyOf[A, B]
  type :@:[A <: Action, B <: Action] = Choice[A, B]
  type :| = Break

  private[sst] def witness[A]: A = null.asInstanceOf[A]
}
