package fpfinal.app

import cats.data._
import cats.implicits._
import fpfinal.app.Configuration._
import fpfinal.common.IO
import fpfinal.service.ExpenseService.ExpenseOp
import fpfinal.service.PersonService.PersonOp

object Syntax {
  implicit class IOOps[A](fa: IO[A]) {
    def toErrorOr: ErrorOr[A] = EitherT.liftF(fa)
    def toSt: St[A] = StateT.liftF(fa.toErrorOr)
    def toAppOp: AppOp[A] = ReaderT.liftF(fa.toSt)
  }

  implicit class PersonOps[A](fa: PersonOp[A]) {
    def toSt: St[A] =
      StateT { appState =>
        val (faS, faA) = fa.run(appState.personState).value
        (appState.copy(personState = faS), faA).pure[ErrorOr]
      }
    def toAppOp: AppOp[A] = ReaderT.liftF(fa.toSt)
  }

  implicit class ValidOps[A](fa: IsValid[A]) {
    def toErrorOr: ErrorOr[A] =
      EitherT.fromEither(
        fa.toEither
          .leftMap { e =>
            s"""Errors: ${e.mkString_("[", "\n", "]")}"""
          }
      )
    def toSt: St[A] = StateT.liftF(fa.toErrorOr)
    def toAppOp: AppOp[A] = ReaderT.liftF(fa.toSt)
  }

  implicit class ExpenseOps[A](fa: ExpenseOp[A]) {
    def toSt: St[A] =
      StateT { appState =>
        val (faS, faA) = fa.run(appState.expenseState).value
        (appState.copy(expenseState = faS), faA).pure[ErrorOr]
      }
    def toAppOp: AppOp[A] = ReaderT.liftF(fa.toSt)
  }
}
