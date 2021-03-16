package fpfinal.app

import cats.data._
import fpfinal.app.Configuration._

object Syntax {
  implicit class IOOps[A](fa: IO[A]) {
    def toErrorOr: ErrorOr[A] = EitherT.liftF(fa)
    def toSt: St[A] = StateT.liftF(fa.toErrorOr)
    def toAppOp: AppOp[A] = ReaderT.liftF(fa.toSt)
  }
}
