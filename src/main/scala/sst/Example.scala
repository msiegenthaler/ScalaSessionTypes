package sst

import sst.SessionTypes._
import sst.TreeSerialization.TS

object Example extends App {

  def printTree[A <: Action : TS](name: String) = {
    println(s"*** $name ***")
    println(TreeSerialization[A])
    println()
  }

  {
    type client = ![String] :>: ?[Int]
    type server = ?[String] :>: ![Int]
    dual[client, server]
    printTree[client]("send/receive")
  }

  {
    type client = ![String] :>: (?[Int] :&: ?[Exception])
    type server = ?[String] :>: (![Int] :@: ![Exception])
    dual[client, server]
    printTree[client]("send/receive with error handling")
  }

  {
    type client = Loop[![String] :>: ?[Int]]
    type server = Loop[?[String] :>: ![Int]]
    dual[client, server]
    printTree[client]("send/receive in loop")
  }

  {
    type client = Loop[(![String] :>: ?[Int]) :@: (![Unit] :>: Break)]
    type server = Loop[(?[String] :>: ![Int]) :&: (?[Unit] :>: Break)]
    dual[client, server]
    printTree[client]("send/receive in loop with break")
  }
}
