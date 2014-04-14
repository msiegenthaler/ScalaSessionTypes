package example

import sst._
import sst.TreeSerialization._
import sst.Opposites._
import sst.ActorIntegration._
import shapeless._
import syntax.typeable._

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

    val actor = new ActorRef("actor-1", {
      case a: Int => s"Hello #-$a"
    })
    val a = RequestResponse[client]
    val resp = a.exec(actor, 1234)
    println(resp.select[String])
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
}
