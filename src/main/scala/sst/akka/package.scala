package sst

import scala.language.implicitConversions
import _root_.akka.actor._
import scala.concurrent.ExecutionContext
import shapeless.Coproduct
import sst.utils.CoproductHandlerIsRunnable

/**
 * Enables usage of akka actors with session typed interaction.
 *
 * Currently supported:
 * - typed ask (AskHandler): ref.ask[MyRequestResponse]
 */
package object akka {
  implicit def askHandler(actor: ActorRef): AskHandler = new AskHandler(actor)

  /** Add send on completed AskHandler. */
  implicit def runnableHandlerActor[Req, Resp <: Coproduct, Rem <: Coproduct, R]
    (rh: ActorRefResponseHandler[Req, Resp, Rem, R])
    (implicit w: CoproductHandlerIsRunnable[Rem], exec: ExecutionContext): RunnableHandlerActor[Req, R] = {
    new RunnableHandlerActor(rh.actor, rh.handle, exec)
  }

}
