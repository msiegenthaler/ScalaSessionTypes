package example.kvstore

import scala.concurrent.duration._
import akka.actor._
import sst._
import Arbiter._
import Replicator._
import Persistence._

/** Node of the key/value store. */
object Replica {
  def props(arbiter: ActorRef, persistenceProps: Props) = Props(new ReplicaActor(arbiter, persistenceProps))

  val insert = "Add a key/value to the kv-store" |>
    send[Insert].receiveAnyOf[OperationAck, OperationFailed]
  val remove = "Remove the association for a key from the store" |>
    send[Remove].receiveAnyOf[OperationAck, OperationFailed]
  val get = "Receive the current value of a key" |>
    send[Get].receive[GetResult]

  val leader = anyOf(insert, remove, get).repeat
  val replica = get.repeat

  case class Insert(key: String, value: String, id: Long)
  case class Remove(key: String, id: Long)
  case class Get(key: String, id: Long)
  case class OperationAck(id: Long)
  case class OperationFailed(id: Long)
  case class GetResult(key: String, valueOption: Option[String], id: Long)


  private class ReplicaActor(arbiter: ActorRef, persistenceProps: Props) extends Actor with ActorLogging {
    val ackTimeout: Duration = 1.second
    val persistenceRetryTimeout: FiniteDuration = 100.millis

    val persistence = context actorOf(persistenceProps, "persistence")
    def persistenceWithRetry() = context actorOf Retry.props(persistenceRetryTimeout)(persistence)

    override def preStart = {
      log.info("Started Replica.")
      arbiter ! Join
    }

    def receive = {
      case JoinedPrimary =>
        log.info("Became Primary.")
        context become leader(Map.empty, Map.empty)
      case JoinedSecondary =>
        log.info("Became Secondary.")
        context become replica(Map.empty, 0)
    }

    object Ids {
      private var replicateId = 0L
      def apply() = {
        replicateId = replicateId + 1
        replicateId
      }
    }

    def leader(replicators: Map[ActorRef, ActorRef], kv: Map[String, String]): Receive = {
      def replicate(id: Long, key: String, value: Option[String]) = {
        val sndr = sender
        val ackManager = context actorOf AckManager.props(
          timeout = ackTimeout,
          onOk = sndr ! OperationAck(id),
          onFailure = sndr ! OperationFailed(id))

        replicators.values.zip(Stream.continually(Ids())) foreach {
          case (replicator, id) =>
            log.debug(s"Replicating with $id ($key=$value)")
            ackManager ! AckManager.Forward(replicator, Replicate(key, value, id))
        }
        ackManager ! AckManager.Forward(persistenceWithRetry, Persist(key, value, Ids()))
        ackManager ! AckManager.InputCompleted
      }

      {
        case Replicas(reps) =>
          //Stop replicators for replicas that left
          val (r2, toRemove) = replicators.span(r => reps.contains(r._1))
          toRemove.values.foreach(context.stop)

          //Start new replicators
          val toAdd = (reps - self -- replicators.keys).map { replica =>
            val replicator = context.actorOf(Props(classOf[Replicator], replica))
            kv.foreach {
              case (k, v) => replicator ! Replicate(k, Some(v), -1)
            }
            log.debug(s"Connection to secondary added: $replica")
            (replica, replicator)
          }.toMap

          context become leader(r2 ++ toAdd, kv)

        case Insert(key, value, id) =>
          log.debug(s"Value $key set to $value")
          context become leader(replicators, kv + (key -> value))
          replicate(id, key, Some(value))

        case Remove(key, id) =>
          log.debug(s"Value $key removed")
          context become (leader(replicators, kv - key))
          replicate(id, key, None)

        case Get(key, id) =>
          sender ! GetResult(key, kv.get(key), id)

        case Replicated(_, -1) => () // ignore, they are from new replicas
      }
    }

    def replica(kv: Map[String, String], expectedSeq: Long): Receive = {
      case Snapshot(key, value, seq) =>
        if (seq == expectedSeq) {
          //new snapshot state
          log.debug(s"Replicated $seq as $key=$value")
          val nkv = value.fold(kv - key)(v => kv + (key -> v))
          context become replica(nkv, seq + 1)

          val sndr = sender
          val ackManager = context actorOf AckManager.props(
            timeout = ackTimeout,
            onOk = sndr ! SnapshotAck(key, seq))
          ackManager ! AckManager.Forward(persistenceWithRetry, Persist(key, value, Ids()))
          ackManager ! AckManager.InputCompleted
        } else if (seq > expectedSeq) {
          //too new, still waiting for other snapshot..
          log.debug(s"Replication request $seq ignored, still waiting for $expectedSeq")
        } else if (seq < expectedSeq) {
          // already got that one
          log.debug(s"Replication request $seq ignored (already applied, at $expectedSeq)")
          sender ! SnapshotAck(key, seq)
        }

      case Get(key, id) =>
        sender ! GetResult(key, kv.get(key), id)
    }
  }
}
