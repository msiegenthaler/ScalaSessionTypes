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

  implicit final class MapCoproductOnAskActorRef[A <: Action, Req, Resp <: Coproduct](ref: AskActorRef[A, Req, Resp]) {
    def mapCoproduct[T](f: CoproductFold.Aux[Resp, Resp, Nothing] => Resp => T)(implicit ec: ExecutionContext): AskActorRef[A, Req, T] =
      ref map f(CoproductFold[Resp])
  }

  implicit def requestSingleResponseHandler[A <: Action](a: A)(implicit rsr: RequestSingleResponse[A]) =
    new RequestSingleResponseHandler[A, rsr.Request, rsr.Response](a, rsr)
  implicit def singleNotificationSubscriptionHandler[A <: Action](a: A)(implicit sns: SingleNotificationSubscription[A]) =
    new SingleNotificationSubscriptionHandler[A, sns.Setup, sns.Message](a, sns)


  implicit def singleRequestResponse[Req: Typeable, Resp]: Service.Aux[Then[?[Req], ![Resp]], Req, Resp] = {
    new SingleServiceHandler[Then[?[Req], ![Resp]]] {
      type Request = Req
      type Response = Resp
      val requestTypeable = implicitly[Typeable[Req]]
    }
  }
  implicit def repeatedSingleRequestResponse[A <: Action](implicit h: SingleServiceHandler[A]): Service.Aux[Repeat[A], h.Request, h.Response] = {
    new SingleServiceHandler[Repeat[A]] {
      type Request = h.Request
      type Response = h.Response
      val requestTypeable = h.requestTypeable
    }
  }
}