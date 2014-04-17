package sst

import scala.language.implicitConversions
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.ActorRef
import akka.util.Timeout
import akka.pattern.ask
import shapeless._
import shapeless.ops.coproduct.Selector
import CoproductOps._

/**
 * Enables usage of akka actors with session typed interaction.
 *
 * Usage syntax when ConvertNumber is a request-response session type:
 * <code>
 * val ref: ActorRef = ???
 * ref.as[ConvertNumber]
 * .handle[Int](identity)
 * .handle[Exception](_ => 0) //not very smart, but hey...
 * .send("123")
 * <code>
 */
object AkkaIntegration {
  /**
   * We use an implicit conversion for .send() to get rid of the HandlerIsRunnable and the ExecutionContext in
   * the method signature of 'send'.
   * This enables to use
   * <code>send("my message")(myTimeout)</code>
   * else you'd have to use
   * <code>send("my message")(myTimeout, implicitly, implicitly)</code>
   */
  class RunnableHandlerActor[Request, R](actor: ActorRef, handler: Any => R, exec: ExecutionContext) {
    def send(value: Request)(implicit timeout: Timeout): Future[R] = {
      implicit val e = exec
      actor.ask(value)(timeout).map(handler)
    }
  }
  implicit def runnableHandlerActor[Req, Resp <: Coproduct, Rem <: Coproduct, R]
    (rh: ActorRefResponseHandler[Req, Resp, Rem, R])
    (implicit w: HandlerIsRunnable[Rem], exec: ExecutionContext): RunnableHandlerActor[Req, R] = {
    new RunnableHandlerActor(rh.actor, rh.handle, exec)
  }

  /** Serves as an interface for RunnableHandlerActor. */
  private[AkkaIntegration] trait ActorRefResponseHandler[Request, Response <: Coproduct, Remaining <: Coproduct, R] {
    private[AkkaIntegration] def actor: ActorRef
    private[AkkaIntegration] def handle(response: Any): R
  }

  implicit final class HandlerActorRef(val actor: ActorRef) {
    def as[A <: Action](implicit rr: RequestResponse[A]) = new RRContainer[A, rr.Request, rr.Response](rr).initial

    final class RRContainer[A <: Action, Request, Response <: Coproduct] private[HandlerActorRef](rr: RequestResponse[A]) {
      private[HandlerActorRef] def initial = new ResponseHandler[Response, Nothing](Handler[Response])

      final class ResponseHandler[Remaining <: Coproduct, R] private[RRContainer](handler: Handler[Response, Remaining, R])
        extends ActorRefResponseHandler[Request, Response, Remaining, R] {
        def handle[A] = new AnyRef {
          def apply[B >: R](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[Response, A]) = {
            val h = handler.handle(f)
            new ResponseHandler(h)
          }
        }
        private[AkkaIntegration] def actor = HandlerActorRef.this.actor
        private[AkkaIntegration] def handle(response: Any) = {
          val typedResponse = rr.parse(response).
            getOrElse(throw new MatchError(s"Unexpected response (wrong type): $response")).
            asInstanceOf[Response] //this is safe, but the compiler cannot prove it
          handler.handler(typedResponse)
        }
      }
    }
  }
}
