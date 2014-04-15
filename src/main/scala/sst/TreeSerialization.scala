package sst

import scala.reflect._


object TreeSerialization {
  def apply[A <: Action : TS] = implicitly[TS[A]].serialize.mkString("\n")

  sealed trait TS[-A <: Action] {
    def serialize: Seq[String]
  }
  implicit def tsSend[V: ClassTag]: TS[Send[V]] = new TS[Send[V]] {
    def serialize = Seq("send " + classTag[V].toString)
  }
  implicit def tsReceive[V: ClassTag]: TS[Receive[V]] = new TS[Receive[V]] {
    def serialize = Seq("receive " + classTag[V].toString)
  }
  implicit def tsChoice[A <: Action : TS, B <: Action : TS]: TS[Choice[A, B]] = new TS[Choice[A, B]] {
    def serialize = "choice" +: (indent[A] ++ indent[B])
  }
  implicit def tsAnyOf[A <: Action : TS, B <: Action : TS]: TS[AnyOf[A, B]] = new TS[AnyOf[A, B]] {
    def serialize = "any of" +: (indent[A] ++ indent[B])
  }
  implicit def tsThen[A <: Action : TS, B <: Action : TS]: TS[Then[A, B]] = new TS[Then[A, B]] {
    def serialize = implicitly[TS[A]].serialize ++ implicitly[TS[B]].serialize
  }
  implicit def tsBreak: TS[Break] = new TS[Break] {
    def serialize = Seq("break")
  }
  implicit def tsRepeat[A <: Action : TS] = new TS[Repeat[A]] {
    def serialize = "repeat" +: indent[A]
  }

  private def indent[A <: Action : TS]: Seq[String] = indent(implicitly[TS[A]].serialize)
  private def indent(v: Seq[String]) = ("- " + v.head) +: v.tail.map("  " + _)
}
