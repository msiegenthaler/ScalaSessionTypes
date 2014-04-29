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


  val convertNumberClient = send[String].receiveAnyOf[Int, Exception]
  val convertNumberServer = Opposite(convertNumberClient)

  def descriptions() = {
    println("client:")
    println(Description.asTree[convertNumberClient.Type])
    println(RequestResponse[convertNumberClient.Type].description)
    println()
    println("server:")
    println(Description.asTree[convertNumberServer.Type])
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
    val convertNumber = convertNumberClient(actor).mapCoproduct(_.
      fold[Int](identity).
      fold[Exception](_ => -1)
    )

    val r1: Int = Await.result(convertNumber ? "123", 1.second)
    println(s"Converting 123 yields $r1")
    val r2: Int = Await.result(convertNumber ? "a2", 1.second)
    println(s"Converting a2 yields $r2")

  }

  descriptions()
  actors()

  akka.shutdown()
}
