package sst

import scala.language.implicitConversions

/**
 * Enables usage of akka actors with session typed interaction.
 *
 * Currently supported:
 * - typed ask (AskHandler): numberConverter(ref).ask("12") //: Future[Int]
 * - typed mapped ask (AskHandler): numberConverter(ref).mapCoproduct(_.fold[Int](identity).fold[String](_ => -1)).ask("12") //: Future[Int]
 */
package object akka extends akka.LowPriorityImplicits {
  /** Ask for single response request/response. */
  implicit def requestSingleResponseHandlerFactory[A <: Action](a: A)(implicit rsr: RequestSingleResponse[A]) =
    new RequestSingleResponseHandlerFactory[A, rsr.Request, rsr.Response](a, rsr)
}
