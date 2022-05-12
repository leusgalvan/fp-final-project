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
     * The important bit is to lift the IO into EitherT correctly. By using attempt,
     * we lift the errors into value space, so we can represent them using Left
     * instead of throwing them. We can then use Right to represent successful values.
     * Notice that the error type for attempt is Throwable but we need String, so
     * we have to make the conversion.
     *
     * This computation does not alter the state and does not read from the environment,
     * so all that is left is to lift it, first to a StateT and then to a ReaderT,
     * by using the corresponding liftF methods.
     */
    def toAppOp: AppOp[A] = {
      val attemptIO: IO[Either[Error, A]] = fa.attempt.map(_.leftMap(_.getMessage))
      val errorOr: ErrorOr[A] = EitherT(attemptIO)
      val st: St[A] = StateT.liftF(errorOr)
      ReaderT.liftF(st)
    }
  }

  implicit class PersonOps[A](fa: PersonOp[A]) {
    /**
      * TODO #25: Translate between a PersonOp and an AppOp.
      *
      * Read the description of toAppOp in ExpenseOps carefully, as this implementation should be
      * really similar.
      */
    def toAppOp: AppOp[A] = {
      val st: St[A] = StateT { appState =>
        val (faS, faA) = fa.run(appState.personState).value
        (appState.copy(personState = faS), faA).pure[ErrorOr]
      }
      ReaderT.liftF(st)
    }
  }

  implicit class ValidOps[A](fa: IsValid[A]) {
    /**
     * Lifts an IsValid[A] into an AppOp[A].
     *
     * We need to turn the IsValid into an Either first, so we can lift it to EitherT.
     * Since we may have many errors arising from validations, we need to merge them
     * and produce a single message that will show them all.
     *
     * Once we do that, the rest is easy. This value does not really need to read from the environment
     * or alter the state so we can just use liftF to lift it into StateT and ReaderT.
     */
    def toAppOp: AppOp[A] = {
      val mergedValidations: Either[Error, A] = fa.toEither.leftMap { errors =>
        s"""Errors: ${errors.mkString_("[", ", ", "]")}"""
      }
      val errorOr: ErrorOr[A] = EitherT.fromEither(mergedValidations)
      val st: St[A] = StateT.liftF(errorOr)
      ReaderT.liftF(st)
    }
  }

  implicit class ExpenseOps[A](fa: ExpenseOp[A]) {
    /**
     * Lifts an ExpenseOp[A] into an AppOp[A].
     *
     * ExpenseOp[A] is an alias for State[ExpenseState, A]. When a value of this type is run,
     * it should not produce exceptions. Furthermore, the type does not encode any information
     * about errors. So we can assume that once we have the A we can lift it to EitherT via pure.
     *
     * The interesting part is in the the StateT layer. We have an ExpenseState and we want to update
     * the ApplicationState with it. We have to be careful not to override the whole application state, though, because
     * we could accidentally lose values!
     *
     * The correct way to update the state is:
     * - Use the current expense state of the application to run our ExpenseOp, so whatever modification we do
     *   is done on top of what we already had.
     * - Once we have our new expense state, update the current application state with it so the
     *   changes are reflected.
     *
     * The last step is easy. We don't need to read from the Environment so we can just lift the value
     * into the ReaderT by using liftF.
     */
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
     * @return an either with Left indicating an error, or Right providing a tuple with the resulting
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
