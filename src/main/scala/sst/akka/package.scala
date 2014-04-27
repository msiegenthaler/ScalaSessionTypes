package sst

import scala.language.implicitConversions
import _root_.akka.actor._
import scala.concurrent.ExecutionContext
import shapeless._
import shapeless.syntax.typeable._
import sst.utils._
import scala.annotation.implicitNotFound

/**
 * Enables usage of akka actors with session typed interaction.
 *
 * Currently supported:
 * - typed ask (AskHandler): numberConverter(ref).ask("12") //: Future[Int]
 */
package object akka {
  /** Ask for single response request/response. */
  implicit def requestSingleResponseHandlerFactory[A <: Action](a: A)(implicit rsr: RequestSingleResponse[A]) =
    new RequestSingleResponseHandlerFactory[A, rsr.Request, rsr.Response](a, rsr)
}
