package fpfinal.app

import cats.data._
import cats.implicits._
import fpfinal.app.Configuration._
import fpfinal.common.IO
import fpfinal.service.ExpenseService.ExpenseOp
import fpfinal.service.PersonService.PersonOp

/**
 * Extension methods for different types in the application.
 */
object Syntax {
  implicit class IOOps[A](fa: IO[A]) {
    /**
     * Lifts an IO[A] into an AppOp[A].
     *
     * An AppOp[A] has three layers, namely (from inner to outer):
     * - An EitherT for error handling: in our case
     * @return
     */
    def toAppOp: AppOp[A] = {
      val errorOr: ErrorOr[A] = EitherT.liftF(fa)
      val st: St[A] = StateT.liftF(errorOr)
      ReaderT.liftF(st)
    }
  }

  implicit class PersonOps[A](fa: PersonOp[A]) {
    /**
      * TODO: Translate between a PersonOp and an AppOp.
      *
      * Make sure the resulting AppState contains the PersonState
      * from this PersonOp.
      */
    def toAppOp: AppOp[A] = ???
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
    /**
     * Syntax method that runs an application with an environment (containing
     * all the needed dependencies such as the services, the controller, etc.)
     * and an initial state, which holds the initial expenses and people
     * (for example the empty state with no people nor expenses).
     *
     * @return an either with Left indicatin an error, or Right providing a tuple with the resulting
     *         state and return value of the application
     */
    def unsafeRunApp(
        environment: Environment,
        initialState: AppState
    ): Either[Error, (AppState, A)] =
      fa.run(environment).run(initialState).value.run

    /**
     * Similar to unsafeRunApp but we only return the state.
     */
    def unsafeRunAppS(
        environment: Environment,
        initialState: AppState
    ): Either[Error, AppState] =
      unsafeRunApp(environment, initialState).map(_._1)
  }
}
