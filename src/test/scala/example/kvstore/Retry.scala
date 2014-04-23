package example.kvstore

import scala.concurrent.duration.FiniteDuration
import akka.actor._
import sst._

/** Retries the request if no response is received in time. */
object Retry {
  def props(interval: FiniteDuration)(target: ActorRef) = Props(new RetryActor(interval, target))

  def retry[A, B] = send[A].receive[B]

  private class RetryActor(interval: FiniteDuration, target: ActorRef) extends Actor {
    override def receive = {
      case msg =>
        context become waitForResponse(sender, msg)
        context.setReceiveTimeout(interval)
    }

    def waitForResponse(src: ActorRef, request: Any): Receive = {
      case ReceiveTimeout =>
        target ! request

      case response =>
        if (sender == src) throw new IllegalStateException("Retry can only be used once.")
        src ! response
        context stop self
    }
  }
}