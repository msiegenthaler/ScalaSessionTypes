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

  println("*** Send Receive in loop ***")
  dual[Loop[![String] :>: ?[Int]],
    Loop[?[String] :>: ![Int]]]
  println(TreeSerialization[
    Loop[![String] :>: ?[Int]]])

  println("*** Send Receive in loop with break ***")
  dual[Loop[(![String] :>: ?[Int]) :@: (![Unit] :>: Break)],
    Loop[(?[String] :>: ![Int]) :&: (?[Unit] :>: Break)]]
  println(TreeSerialization[
    Loop[(![String] :>: ?[Int]) :@: (![Unit] :>: Break)]])
}
