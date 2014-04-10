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
}
