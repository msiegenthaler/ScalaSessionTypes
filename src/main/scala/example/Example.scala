package example

import scala.language.reflectiveCalls
import sst._
import sst.TreeSerialization._
import sst.Opposites._
import sst.ActorIntegration._
import shapeless._
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

    val actor = new ActorRef("actor-1", {
      case a: Int => s"Hello #-$a"
    })
    val resp = RequestResponse[client].exec(actor, 1234)
    println(resp.select[String])
    println(Handler(resp).handleTyped[String](identity).run())
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

    val actor = new ActorRef("actor-2", {
      case a: String => try {
        a.toInt
      } catch {
        case e: NumberFormatException => e
      }
    })
    val resp1 = RequestResponse[client].exec(actor, "1234")
    println(resp1.select[Int])
    println(resp1.select[Exception])
    val resp2 = RequestResponse[client].exec(actor, "2a")
    println(resp2.select[Int])
    println(resp2.select[Exception])

    println(RequestResponse[client].handle(actor, "123").
      handleTyped[Int](identity).
      handleTyped[Exception](e => 0).
      run())
    println(RequestResponse[client].handle(actor, "a2").
      handleTyped[Int](identity).
      handleTyped[Exception](e => 0).
      run())
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
