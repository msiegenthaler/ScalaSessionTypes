package example.kvstore

import scala.util.Random
import akka.actor._
import sst._

/** Store a key/value pair. Might fail. */
object Persistence {
  def props(flaky: Boolean) = Props(classOf[Persistence], flaky)

  val persist = send[Persist].receive[Persisted].repeat
  case class Persist(key: String, valueOption: Option[String], id: Long)
  case class Persisted(key: String, id: Long)

  private class Persistence(flaky: Boolean) extends Actor {
    def receive = {
      case Persist(key, _, id) =>
        if (!flaky || Random.nextBoolean()) sender ! Persisted(key, id)
        else throw new PersistenceException
    }
  }
  case class PersistenceException() extends Exception("Persistence failure")
}