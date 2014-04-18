package example

import scala.language.reflectiveCalls
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import akka.actor._
import akka.util.Timeout
import sst._
import sst.TreeSerialization._
import sst.Opposites._
import sst.AkkaIntegration._

object Example extends App {
  def printTree[A <: Action : TS](name: String) = {
    println()
    println(s"*** $name ***")
    println(TreeSerialization[A])
  }

  {
    type client = ![Int] :>: ?[String]
    type server = ?[Int] :>: ![String]
    Opposite.is[client, server]
    val opC = Opposite[client]
    val opS = Opposite[server]
    implicitly[server =:= opC.Out]

    printTree[client]("send/receive")
    println(RequestResponse[client].description)
    println(RequestResponse[opS.Out].description)

    val r = RequestResponse[client].parse("Hello").get
    r.select[String]
  }

  {
    type client = ![String] :>: (?[Int] :&: ?[Exception])
    type server = ?[String] :>: (![Int] :@: ![Exception])
    Opposite.is[client, server]
    val opC = Opposite[client]
    val opS = Opposite[server]
    implicitly[server =:= opC.Out]
    printTree[client]("send/receive with error handling")
    println(RequestResponse[client].description)
    println(RequestResponse[opS.Out].description)
  }

  {
    type client = ![String] :>: (?[Int] :&: ?[String] :&: ?[Exception])
    type server = ?[String] :>: (![Int] :@: ![String] :@: ![Exception])
    Opposite.is[client, server]
    val opC = Opposite[client]
    val opS = Opposite[server]
    implicitly[server =:= opC.Out]
    printTree[client]("send/receive with 3 elements")
    println(RequestResponse[client].description)
    println(RequestResponse[opS.Out].description)
  }

  {
    type client = ![String] :>: (?[Int] :&: ?[Long] :&: ?[String] :&: ?[Exception])
    type server = ?[String] :>: (![Int] :@: ![Long] :@: ![String] :@: ![Exception])
    Opposite.is[client, server]
    val opC = Opposite[client]
    val opS = Opposite[server]
    implicitly[server =:= opC.Out]
    printTree[client]("send/receive with 4 elements")
    println(RequestResponse[client].description)
    println(RequestResponse[opS.Out].description)
  }

  {
    type client = ![String] :>: (((?[Int] :&: ?[Long]) :&: ?[String]) :&: ?[Exception])
    type server = ?[String] :>: (((![Int] :@: ![Long]) :@: ![String]) :@: ![Exception])
    Opposite.is[client, server]
    val opC = Opposite[client]
    val opS = Opposite[server]
    implicitly[server =:= opC.Out]
    printTree[client]("send/receive with 4 elements left-bound")
    println(RequestResponse[client].description)
    println(RequestResponse[opS.Out].description)
  }

  /*
  //Not yet supported
  {
    type client = ![String] :>: ((?[Int] :&: ?[Long]) :&: (?[String] :&: ?[Exception]))
    type server = ?[String] :>: ((![Int] :@: ![Long]) :@: (![String] :@: ![Exception]))
    Opposite.is[client, server]
    val opC = Opposite[client]
    val opS = Opposite[server]
    implicitly[server =:= opC.Out]
    printTree[client]("send/receive with 4 elements grouped")
    println(RequestResponse[client].description)
    println(RequestResponse[opS.Out].description)
  }
  */

  {
    type client = Repeat[![String] :>: ?[Int]]
    type server = Repeat[?[String] :>: ![Int]]
    Opposite.is[client, server]
    val opC = Opposite[client]
    implicitly[server =:= opC.Out]
    printTree[client]("send/receive in loop")
  }

  {
    type client = Repeat[(![String] :>: ?[Int]) :@: (![Unit] :>: Break)]
    type server = Repeat[(?[String] :>: ![Int]) :&: (?[Unit] :>: Break)]
    Opposite.is[client, server]
    val opC = Opposite[client]
    implicitly[server =:= opC.Out]
    printTree[client]("send/receive in loop with break")
  }

  {
    val x = send[Int].
      send[Long].receiveAnyOf2[String, Exception].answer[Unit].
      chooseFrom(
        send[Int].receive[String],
        send[String].receive[Int])
    Opposite[x.Type]
  }

  val akka = ActorSystem("Example")
  implicit val defaultTimeout: Timeout = 1.second

  def actors() = {
    val cn = Action.send[String].receiveAnyOf[Int, Exception]
    type ConvertNumber = cn.Type

    class MyActor extends Actor {
      def receive = {
        case a: String =>
          val result = try {
            a.toInt
          } catch {
            case e: NumberFormatException => e
          }
          sender ! result
      }
    }
    val actor: ActorRef = akka.actorOf(Props(new MyActor))

    println("Converting numbers")
    val convertNumber = actor.as[ConvertNumber]
      .handle[Int](identity)
      .handle[Exception](_ => -1)


    val r1: Int = Await.result(convertNumber.send("123"), 1.second)
    println(s"Converting 123 yields $r1")
    val r2: Int = Await.result(convertNumber.send("a2"), 1.second)
    println(s"Converting a2 yields $r2")

  }
  actors()

  akka.shutdown()
}
