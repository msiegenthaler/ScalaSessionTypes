package sst

import SessionTypes._

object Example extends App {

  println("*** Send Receive ***")
  dual[
    ![String] :>: ?[Int],
    ?[String] :>: ![Int]]
  println(TreeSerialization[
    ![String] :>: ?[Int]])

  println("*** Send Receive with error handling ***")
  dual[
    ![String] :>: (?[Int] :&: ?[Exception]),
    ?[String] :>: (![Int] :@: ![Exception])]
  println(TreeSerialization[
    ![String] :>: (?[Int] :&: ?[Exception])])


  println("*** Loop ***")
  type RR = ![String] :>: ?[Int]
  trait A_c extends Cons[?[Int], A_c]
  trait A_s extends Cons[![Int], A_s]
  dual[A_c, A_s]
  println(TreeSerialization[A_c])
}
