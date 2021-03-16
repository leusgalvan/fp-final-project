package fpfinal.app

import cats._
import cats.implicits._
import cats.data._
import fpfinal.app.Configuration._
import fpfinal.service.PersonService.{PersonOp, PersonState}

object Syntax {
  val evalToErrorOr: Eval ~> ErrorOr = new (Eval ~> ErrorOr) {
    override def apply[A](fa: Eval[A]): ErrorOr[A] = fa.value.asRight[Error]
  }

  def personStateToAppState(personState: PersonState): AppState
  def personToAppOp[A](personOp: PersonOp[A]): AppOp[A] = {
    val x = readEnv.mapF(stEnv => stEnv.get)
    for {
      env <- readEnv
      state <- State.get[AppState]
    } yield ()
    ReaderT.apply[St, Environment, A](_ => StateT)
    val x = personOp.mapK[ErrorOr](evalToErrorOr)
  }

  implicit class IOOps[A](fa: IO[A]) {
    def toErrorOr: ErrorOr[A] = EitherT.liftF(fa)
    def toSt: St[A] = StateT.liftF(fa.toErrorOr)
    def toAppOp: AppOp[A] = ReaderT.liftF(fa.toSt)
  }
}
