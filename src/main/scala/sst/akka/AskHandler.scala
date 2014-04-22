package sst.akka

import scala.concurrent._
import akka.actor.ActorRef
import akka.util.Timeout
import akka.pattern.ask
import shapeless.Coproduct
import shapeless.ops.coproduct.Selector
import sst.{RequestResponse, Action}
import sst.utils.CoproductHandler
import sst.utils.CoproductOps._

/**
 * Ask pattern for session types.
 *
 * Usage syntax where ConvertNumber is a request-response session type:
 * <code>
 * val ref: ActorRef = ???
 * ref.ask[ConvertNumber]
 * .handle[Int](identity)
 * .handle[Exception](_ => 0) //not very smart, but hey...
 * .send("123")
 * <code>
 */
final class AskHandler(val actor: ActorRef) {
  def ask[A <: Action](implicit rr: RequestResponse[A]) = new Container[A, rr.Request, rr.Response](rr).initial

  final class Container[A <: Action, Request, Response <: Coproduct] private[AskHandler](rr: RequestResponse[A]) {
    private[AskHandler] def initial = new ResponseHandler[Response, Nothing](CoproductHandler[Response])

    final class ResponseHandler[Remaining <: Coproduct, R] private[Container](handler: CoproductHandler[Response, Remaining, R])
      extends AskResponseHandler[Request, Response, Remaining, R] {
      def handle[A] = new AnyRef {
        def apply[B >: R](f: A => B)(implicit r: Remove[Remaining, A], contains: Contains.Yes[Remaining, A], s: Selector[Response, A]) = {
          val h = handler.handle(f)
          new ResponseHandler(h)
        }
      }
      private[akka] def actor = AskHandler.this.actor
      private[akka] def handle(response: Any) = {
        val typedResponse = rr.parse(response).
          getOrElse(throw new MatchError(s"Unexpected response (wrong type): $response")).
          asInstanceOf[Response] //this is safe, but the compiler cannot prove it
        handler.handler(typedResponse)
      }
    }
  }
}

/**
 * We use an implicit conversion for .send() to get rid of the HandlerIsRunnable and the ExecutionContext in
 * the method signature of 'send'.
 * This enables to use
 * <code>send("my message")(myTimeout)</code>
 * else you'd have to use
 * <code>send("my message")(myTimeout, implicitly, implicitly)</code>
 */
class RunnableAskHandler[Request, R](actor: ActorRef, handler: Any => R, exec: ExecutionContext) {
  def send(value: Request)(implicit timeout: Timeout): Future[R] = {
    implicit val e = exec
    actor.ask(value)(timeout).map(handler)
  }
}

/** Serves as an interface for RunnableHandlerActor. */
private[akka] trait AskResponseHandler[Request, Response <: Coproduct, Remaining <: Coproduct, R] {
  private[akka] def actor: ActorRef
  private[akka] def handle(response: Any): R
}
