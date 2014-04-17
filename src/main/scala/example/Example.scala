package example

import scala.language.reflectiveCalls
import sst._
import sst.TreeSerialization._
import sst.Opposites._
import sst.ActorIntegration._
import Handle._

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
}
