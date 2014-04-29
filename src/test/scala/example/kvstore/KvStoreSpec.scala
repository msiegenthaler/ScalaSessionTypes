package example.kvstore

import scala.language.reflectiveCalls
import scala.concurrent._
import akka.actor._
import akka.testkit._
import akka.util._
import shapeless._
import org.specs2.mutable._
import org.specs2.specification.Context
import sst.akka._
import sst.utils.CoproductFold
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
      val f = Arbiter.getLeader(arbiter) ? Arbiter.GetLeader
      Await.result(f, timeout.duration).
        getOrElse(throw new RuntimeException("no leader"))
    }
    def replicas = nodes.filterNot(_ == leader)
  }

  "KV-Store" should {
    val ackFailed = CoproductFold[OperationAck :+: OperationFailed :+: CNil]
      .fold[OperationAck](_ => true)
      .fold[OperationFailed](_ => false)
    "successfully store a value" in new kvstore {
      val f = insert(leader).map(ackFailed) ? Insert("key", "value", 1)
      valueOf(f) must beTrue
      system.shutdown()
    }
    "deliver a stored value at leader" in new kvstore {
      val i1 = insert(leader).map(ackFailed) ? Insert("key1", "my value", 1)
      valueOf(i1) must beTrue

      val g1 = get(leader).map(_.valueOption) ? Get("key1", 2)
      valueOf(g1) must_== Some("my value")

      system.shutdown()
    }
    "deliver a stored value at replica" in new kvstore {
      val i1 = insert(leader).map(ackFailed) ? Insert("key1", "my value", 1)
      valueOf(i1) must beTrue

      val g1 = get(replicas.head) ? Get("key1", 2)
      valueOf(g1.map(_.valueOption)) must_== Some("my value")

      system.shutdown()
    }
  }
}
