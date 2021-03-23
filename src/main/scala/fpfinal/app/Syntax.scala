package fpfinal.app

import cats.data._
import cats.implicits._
import fpfinal.app.Configuration._
import fpfinal.common.IO
import fpfinal.service.ExpenseService.ExpenseOp
import fpfinal.service.PersonService.PersonOp

object Syntax {
  implicit class IOOps[A](fa: IO[A]) {
    def toAppOp: AppOp[A] = {
      val errorOr: ErrorOr[A] = EitherT.liftF(fa)
      val st: St[A] = StateT.liftF(errorOr)
      ReaderT.liftF(st)
    }
  }

  implicit class PersonOps[A](fa: PersonOp[A]) {
    def toAppOp: AppOp[A] = {
      val st: St[A] =
        StateT { appState =>
          val (faS, faA) = fa.run(appState.personState).value
          (appState.copy(personState = faS), faA).pure[ErrorOr]
        }
      ReaderT.liftF(st)
    }
  }

  implicit class ValidOps[A](fa: IsValid[A]) {
    def toAppOp: AppOp[A] = {
      val errorOr: ErrorOr[A] = {
        EitherT.fromEither(
          fa.toEither
            .leftMap { e =>
              s"""Errors: ${e.mkString_("[", ", ", "]")}"""
            }
        )

      }
      val st: St[A] = StateT.liftF(errorOr)
      ReaderT.liftF(st)
    }
  }

  implicit class ExpenseOps[A](fa: ExpenseOp[A]) {
    def toAppOp: AppOp[A] = {
      val st: St[A] = StateT { appState =>
        val (faS, faA) = fa.run(appState.expenseState).value
        (appState.copy(expenseState = faS), faA).pure[ErrorOr]
      }
      ReaderT.liftF(st)
    }
  }

  implicit class AppOps[A](fa: AppOp[A]) {
    def unsafeRunApp(
        environment: Environment,
        initialState: AppState
    ): Either[Error, (AppState, A)] =
      fa.run(environment).run(initialState).value.run

    def unsafeRunAppS(
        environment: Environment,
        initialState: AppState
    ): Either[Error, AppState] =
      unsafeRunApp(environment, initialState).map(_._1)
  }
}
