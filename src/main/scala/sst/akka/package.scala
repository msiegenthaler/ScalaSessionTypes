package sst

import scala.language.reflectiveCalls
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.concurrent.{ExecutionContext, Future}
import shapeless._
import sst.utils._

/**
 * Enables usage of akka actors with session typed interaction.
 *
 * Currently supported:
 * - typed ask (AskHandler): numberConverter(ref).ask("12") //: Future[Int]
 * - typed mapped ask (AskHandler): numberConverter(ref).mapCoproduct(_.fold[Int](identity).fold[String](_ => -1)).ask("12") //: Future[Int]
 */
package object akka extends akka.LowPriorityImplicits {
  //Add mapCoproduct to certain types
  type Functor[F[A], A] = {
    def map[B](a: A => B): F[B]
  }
  implicit final class MapCoproductOnFunctor[F[Z] <: Functor[F, Z], C <: Coproduct](functor: F[C]) {
    def mapCoproduct[T](f: CoproductFold.Aux[C, C, Nothing] => C => T): F[T] =
      functor map f(CoproductFold[C])
  }
  /** i.e. a Future. */
  type FunctorEC[F[A], A] = {
    def map[B](a: A => B)(implicit ec: ExecutionContext): F[B]
  }
  implicit final class MapCoproductOnFunctorEC[F[Z] <: FunctorEC[F, Z], C <: Coproduct](functor: F[C]) {
    def mapCoproduct[T](f: CoproductFold.Aux[C, C, Nothing] => C => T)(implicit ec: ExecutionContext): F[T] =
      functor map f(CoproductFold[C])
  }
  implicit final class MapCoproductOnAskActorRef[A <: Action, Req, Resp <: Coproduct](functor: AskActorRef[A, Req, Resp]) {
    def mapCoproduct[T](f: CoproductFold.Aux[Resp, Resp, Nothing] => Resp => T)(implicit ec: ExecutionContext): AskActorRef[A, Req, T] =
      functor map f(CoproductFold[Resp])
  }

  implicit def requestSingleResponseHandler[A <: Action](a: A)(implicit rsr: RequestSingleResponse[A]) =
    new RequestSingleResponseHandler[A, rsr.Request, rsr.Response](a, rsr)
}