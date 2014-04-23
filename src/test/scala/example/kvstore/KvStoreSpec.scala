package example.kvstore

import scala.language.reflectiveCalls
import scala.concurrent._
import akka.actor._
import akka.testkit._
import akka.util._
import org.specs2.mutable._
import org.specs2.specification.Context
import sst.akka._
import Replica._

class KvStoreSpec extends Specification {
  abstract class actors extends TestKit(ActorSystem("AskHandlerSpec")) with Context with After {
    override def after = ()
  }
  abstract class kvstore extends actors {
    def valueOf[A](f: Future[A]) = Await.result(f, timeout.duration)

    implicit val timeout: Timeout = 2000
    val arbiter = system.actorOf(Arbiter.props, "arbiter")
    val persistence = Persistence.props(true)
    val nodes = (1 to 3).map(i => system.actorOf(Replica.props(arbiter, persistence), s"node-$i"))
    def leader = {
      val f = arbiter.ask[Arbiter.getLeader.Type].
        handle[Option[ActorRef]](identity).
        send(Arbiter.GetLeader)
      Await.result(f, timeout.duration).
        getOrElse(throw new RuntimeException("no leader"))
    }
    def replicas = nodes.filterNot(_ == leader)
  }

  "KV-Store" should {
    "successfully store a value" in new kvstore {
      val f = leader.ask[insert.Type].
        handle[OperationAck](a => a.id).
        handle[OperationFailed](_ => -1).
        send(Insert("key", "value", 1))

      valueOf(f) must_== 1
      system.shutdown()
    }
    "deliver a stored value at leader" in new kvstore {
      val i1 = leader.ask[insert.Type].
        handle[OperationAck](a => a.id).
        handle[OperationFailed](_ => -1).
        send(Insert("key1", "my value", 1))
      valueOf(i1) must_== 1

      val g1 = leader.ask[get.Type].
        handle[GetResult](_.valueOption).
        send(Get("key1", 2))
      valueOf(g1) must_== Some("my value")

      system.shutdown()
    }
    "deliver a stored value at replica" in new kvstore {
      val i1 = leader.ask[insert.Type].
        handle[OperationAck](a => a.id).
        handle[OperationFailed](_ => -1).
        send(Insert("key1", "my value", 1))
      valueOf(i1) must_== 1

      val g1 = replicas.head.ask[get.Type].
        handle[GetResult](_.valueOption).
        send(Get("key1", 2))
      valueOf(g1) must_== Some("my value")

      system.shutdown()
    }
  }
}
