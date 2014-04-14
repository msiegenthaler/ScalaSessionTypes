package example

import sst.SessionTypes._
import sst.TreeSerialization.TS
import sst.{ActorIntegration, TreeSerialization}
import sst.ActorIntegration._

object Example extends App {

  def printTree[A <: Action : TS](name: String) = {
    println()
    println(s"*** $name ***")
    println(TreeSerialization[A])
  }

  {
    type client = ![String] :>: ?[Int]
    type server = ?[String] :>: ![Int]
    dual[client, server]
    printTree[client]("send/receive")
    println(requestResponse[client].description)
  }

  {
    type client = ![String] :>: (?[Int] :&: ?[Exception])
    type server = ?[String] :>: (![Int] :@: ![Exception])
    dual[client, server]
    printTree[client]("send/receive with error handling")
    println(requestResponse[client].description)
  }

  {
    type client = Repeat[![String] :>: ?[Int]]
    type server = Repeat[?[String] :>: ![Int]]
    dual[client, server]
    printTree[client]("send/receive in loop")
  }

  {
    type client = Repeat[(![String] :>: ?[Int]) :@: (![Unit] :>: Break)]
    type server = Repeat[(?[String] :>: ![Int]) :&: (?[Unit] :>: Break)]
    dual[client, server]
    printTree[client]("send/receive in loop with break")
  }
}
