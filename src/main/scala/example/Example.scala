package example

import scala.language.reflectiveCalls
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import akka.actor._
import akka.util.Timeout
import sst._
import sst.Description._
import sst.akka._

object Example extends App {
  val akka = ActorSystem("Example")
  implicit val defaultTimeout: Timeout = 1.second


  type ConvertNumberClient = ![String] :>: (?[Int] :&: ?[Exception])
  val server = Opposite[ConvertNumberClient]
  type ConvertNumberServer = server.Out


  def descriptions() = {
    println("client:")
    println(Description.asTree[ConvertNumberClient])
    println(RequestResponse[ConvertNumberClient].description)
    println()
    println("server:")
    println(Description.asTree[ConvertNumberServer])
    println()
  }

  def actors() = {
    class ConverterActor extends Actor {
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
    val actor: ActorRef = akka.actorOf(Props(new ConverterActor))

    println("Converting numbers")
    val convertNumber = actor.ask[ConvertNumberClient]
      .handle[Int](identity)
      .handle[Exception](_ => -1)


    val r1: Int = Await.result(convertNumber.send("123"), 1.second)
    println(s"Converting 123 yields $r1")
    val r2: Int = Await.result(convertNumber.send("a2"), 1.second)
    println(s"Converting a2 yields $r2")

  }

  descriptions()
  actors()

  akka.shutdown()
}
