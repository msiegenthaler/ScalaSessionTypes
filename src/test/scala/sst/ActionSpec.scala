package sst

import org.specs2.mutable._

class ActionSpec extends Specification {
  private def isOk = true must_== true

  "Action" should {
    "be buildable on typelevel for client" in {
      type Client1 = ![String]
      type Client2 = ![String] :>: ?[Int]
      type Client3 = ![String] :>: (?[String] :&: ?[Int])
      type Client4 = Repeat[![String] :>: (?[String] :&: ?[Int])]
      isOk
    }
    "be buildable on typelevel for server" in {
      type Server1 = ?[String]
      type Server2 = ?[String] :>: ![Int]
      type Server3 = ?[String] :>: (![String] :@: ![Int])
      type Server4 = Repeat[?[String] :>: (![String] :@: ![Int])]
      isOk
    }

    "be buildable on instance level for client" in {
      val client1 = send[String]
      client1 must not beNull
      val client2 = send[String].receive[Int]
      client2 must not beNull
      val client3 = send[String].receiveAnyOf[String, Int]
      client3 must not beNull
      val client4 = send[String].receiveAnyOf[String, Int].repeat
      client4 must not beNull
      val client5 = send[String].receiveAnyOf3[String, Int, Long].repeat
      client5 must not beNull
    }
    "be buildable on instance level for server" in {
      val server1 = receive[String]
      server1 must not beNull
      val server2 = receive[String].send[Int]
      server2 must not beNull
      val server3 = receive[String].answerEither[String, Int]
      server3 must not beNull
      val server4 = receive[String].answerEither[String, Int].repeat
      server4 must not beNull
      val server5 = receive[String].answerEither[String, Int].repeat
      server5 must not beNull
      val server6 = receive[String].chooseFrom(answer[String].receive[Unit], send[Long].repeat)
      server6 must not beNull
    }

    "be described by a text before the definition" in {
      val converter = "Convert a string to a number" |> send[String].receive[Int]
      converter.description must_== Some("Convert a string to a number")
    }
    "be described by action.describe" in {
      val numberGenerator = send[Unit].repeat(receive[Int]) describe "Sends one number per second to the sender of ()"
      numberGenerator.description must_== Some("Sends one number per second to the sender of ()")
    }
    "be described by text after the definition" in {
      val adder = send[(Int,Int)].receive[Int] <| "Add two numbers"
      adder.description must_== Some("Add two numbers")
    }
  }
}
