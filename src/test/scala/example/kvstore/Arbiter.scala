package example.kvstore

import akka.actor._
import sst._

/** Responsible to elect a leader and the make all replicas known to the leader. */
object Arbiter {
  def props = Props(new ArbiterActor)

  val join = send[Join.type].anyOf(
    receive[JoinedPrimary.type].andThen(receive[Replicas]),
    receive[JoinedSecondary.type])

  case object Join
  case object JoinedPrimary
  case object JoinedSecondary
  case class Replicas(replicas: Set[ActorRef])

  private class ArbiterActor extends Actor {
    var leader: Option[ActorRef] = None
    var replicas = Set.empty[ActorRef]

    def receive = {
      case Join =>
        if (leader.isEmpty) {
          leader = Some(sender)
          replicas += sender
          sender ! JoinedPrimary
        } else {
          replicas += sender
          sender ! JoinedSecondary
        }
        leader foreach (_ ! Replicas(replicas))
    }
  }
}
