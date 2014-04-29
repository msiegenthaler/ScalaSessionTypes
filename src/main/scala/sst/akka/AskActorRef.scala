package sst.akka

import scala.concurrent._
import akka.actor._
import akka.util.Timeout
import sst.Action

/** ActorRef wrapper for request/response actions. */
abstract class AskActorRef[A <: Action, -Request, +Response](val action: Action, val actor: ActorRef) {
  def ask(msg: Request)(implicit timeout: Timeout, ec: ExecutionContext): Future[Response]
  def ?(msg: Request)(implicit timeout: Timeout, ec: ExecutionContext) = ask(msg)

  def map[T](f: Response => T)(implicit ec: ExecutionContext): AskActorRef[A, Request, T] = {
    val outer = this
    new AskActorRef[A, Request, T](action, actor) {
      override def ask(msg: Request)(implicit timeout: Timeout, ec: ExecutionContext) = outer.ask(msg) map f
    }
  }

  override def toString = s"$actor as $action"
}