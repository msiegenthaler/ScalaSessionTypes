package sst

import scala.reflect._
import SessionTypes._


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
  implicit def tsCons[A <: Action : TS, B <: Action : TS]: TS[Cons[A, B]] = new TS[Cons[A, B]] {
    def serialize = implicitly[TS[A]].serialize ++ implicitly[TS[B]].serialize
  }

  private def indent[A <: Action : TS]: Seq[String] = indent(implicitly[TS[A]].serialize)
  private def indent(v: Seq[String]) = ("- " + v.head) +: v.tail.map("  " + _)
}
