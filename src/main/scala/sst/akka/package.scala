package sst

import scala.language.implicitConversions
import _root_.akka.actor._
import scala.concurrent.ExecutionContext
import shapeless.Coproduct
import sst.utils.CoproductMapperIsComplete

/**
 * Enables usage of akka actors with session typed interaction.
 *
 * Currently supported:
 * - typed ask (AskHandler): ref.ask[MyRequestResponse]
 */
package object akka {
  implicit def askHandler(actor: ActorRef): AskHandler = new AskHandler(actor)
  implicit def subscriptionHandler(actor: ActorRef): SubscriptionHandler = new SubscriptionHandler(actor)

  /** Add send on completed AskHandler. */
  implicit def runnableAskHandler[Req, Resp <: Coproduct, Rem <: Coproduct, R](rh: AskResponseHandler[Req, Resp, Rem, R])
    (implicit w: CoproductMapperIsComplete[Rem], exec: ExecutionContext): RunnableAskHandler[Req, R] = {
    new RunnableAskHandler(rh.actor, rh.handle, exec)
  }

}
