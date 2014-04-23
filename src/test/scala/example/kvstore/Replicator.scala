package example.kvstore

import scala.concurrent.duration._
import scala.collection.immutable.Queue
import akka.actor._
import sst._

object Replicator {
  def props(replica: ActorRef) = Props(new Replicator(replica))

  val replicate = send[Replicate].receive[Replicated].repeat
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  val getSnapshot = receive[Snapshot].send[SnapshotAck].repeat
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)


  class Replicator(val replica: ActorRef) extends Actor with ActorLogging {
    import context.dispatcher

    val resendAfter: FiniteDuration = 50.millis

    case object Resend
    case class Pending(seq: Long, key: String, value: Option[String], sender: ActorRef, senderRef: Long, t: Long)

    var pending = Queue.empty[Pending]
    object Seq {
      private var i = 0L
      def apply() = {
        val ret = i
        i = i + 1
        ret
      }
    }

    override def postStop = {
      pending.foreach(ack) // so that nobody waits for us
      scheduled.foreach(_.cancel)
    }

    var scheduled: Option[Cancellable] = None

    def cancelResend = {
      scheduled.foreach(_.cancel)
      scheduled = None
    }
    def scheduleResend(after: FiniteDuration) = {
      cancelResend
      val s = context.system.scheduler.scheduleOnce(resendAfter, self, Resend)
      scheduled = Some(s)
    }

    def receive = {
      case Replicate(key, value, id) =>
        log.debug(s"Received Replication Request $id ($key=$value)")
        val wasEmpty = pending.isEmpty
        val msg = Pending(Seq(), key, value, sender, id, System.currentTimeMillis)
        pending = pending enqueue msg
        if (wasEmpty) onHeadModified
        //try to optimize by already sending it.. if nothing gets lost then we win
        sendToReplica(msg)

      case SnapshotAck(key, seq) =>
        log.debug(s"Replica confirmed replication of $seq (key=$key)")
        val (toAck, remainder) = pending.span(_.seq <= seq)
        if (toAck.nonEmpty) {
          toAck.foreach(ack)
          pending = remainder
          onHeadModified
        }

      case Resend =>
        if (pending.nonEmpty)
          log.debug(s"Resending ${pending.size} outstanding replication requests")
        cancelResend
        pending.headOption.foreach { msg =>
          sendToReplica(msg)
          scheduleResend(resendAfter)
        }
    }

    def sendToReplica(p: Pending) = replica ! Snapshot(p.key, p.value, p.seq)

    def ack(p: Pending) = {
      log.debug(s"Replication ack for key ${p.senderRef}")
      p.sender ! Replicated(p.key, p.senderRef)
    }

    def onHeadModified = if (pending.nonEmpty) {
      val sinceEnqueued = (System.currentTimeMillis - pending.head.t).millis
      cancelResend
      if (sinceEnqueued > resendAfter) self ! Resend
      else scheduleResend(resendAfter - sinceEnqueued)
    }
  }
}
