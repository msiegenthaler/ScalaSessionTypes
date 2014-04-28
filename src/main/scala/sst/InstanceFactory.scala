package sst

trait InstanceFactory[A <: Action] {
  def apply(): A
}
object InstanceFactory {
  implicit def _send[A]: InstanceFactory[Send[A]] = make(Send[A]())
  implicit def _receive[A]: InstanceFactory[Receive[A]] = make(Receive[A]())
  implicit def _then[A <: Action, B <: Action](implicit a: InstanceFactory[A], b: InstanceFactory[B]): InstanceFactory[Then[A, B]] =
    make(Then(a(), b()))
  implicit def _anyOf[A <: Action, B <: Action](implicit a: InstanceFactory[A], b: InstanceFactory[B]): InstanceFactory[AnyOf[A, B]] =
    make(AnyOf(a(), b()))
  implicit def _choice[A <: Action, B <: Action](implicit a: InstanceFactory[A], b: InstanceFactory[B]): InstanceFactory[Choice[A, B]] =
    make(Choice(a(), b()))
  implicit def _repeat[A <: Action](implicit a: InstanceFactory[A]): InstanceFactory[Repeat[A]] = make(Repeat(a()))
  implicit def _break: InstanceFactory[Break] = make(Break())

  private def make[A <: Action](f: => A): InstanceFactory[A] = new InstanceFactory[A] {
    override def apply() = f
  }
}
