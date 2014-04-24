package example.kvstore

import scala.concurrent.duration._
import akka.actor._
import sst._

/** Handles one or more messages that must be ack'd. Either onOk or onFailure is called. */
object AckManager {
  def props(timeout: Duration = Duration.Undefined, onOk: => Unit = (), onFailure: => Unit = ()) = {
    Props(new AckManagerActor(timeout, onOk, onFailure))
  }

  val input = """|Forwards messages (to ! msg) and keeps track of acks. As soon as InputCompleted and all acks are
                 |received then onOk is called. If not all acks are received within timeout (after InputCompleted)
                 |then onFailure is called.""".stripMargin |>
    repeat(send[Forward]).send[InputCompleted.type]
  case class Forward(to: ActorRef, msg: Any)
  case object InputCompleted

  class AckManagerActor(timeout: Duration, onOk: => Unit, onFailure: => Unit) extends Actor with Stash with ActorLogging {
    def receive = awaitInput(Set.empty)

    def awaitInput(in: Set[Long]): Receive = {
      case Forward(to, msg) =>
        val token = idExtractor(msg)
        context become awaitInput(in + token)
        to ! msg

      case InputCompleted =>
        if (in.isEmpty) completed()
        else {
          context.setReceiveTimeout(timeout)
          unstashAll
          context become awaitAcks(in)
        }

      case other => stash
    }

    def awaitAcks(remaining: Set[Long]): Receive = {
      idExtractor.andThen { token =>
        val r2 = remaining - token
        if (r2.isEmpty) completed()
        else context become awaitAcks(r2)
      } orElse {
        case ReceiveTimeout =>
          log.debug(s"Failed to obtain all confirmations in time. Outstanding are: $remaining")
          onFailure
          context stop self
      }
    }

    def completed() = {
      onOk
      context stop self
    }

    def idExtractor: PartialFunction[Any, Long] = {
      case Replicator.Replicate(_, _, id) => id
      case Replicator.Replicated(_, id) => id
      case Persistence.Persist(_, _, id) => id
      case Persistence.Persisted(_, id) => id
    }
  }
}