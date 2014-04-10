package sst

import SessionTypes._

object Example {

  dual[
    ![String] :>: ?[Int],
    ?[String] :>: ![Int]]

  dual[
    ![String] :>: (?[Int] :&: ?[Exception]),
    ?[String] :>: (![Int] :@: ![Exception])]
}
