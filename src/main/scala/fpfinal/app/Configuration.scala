package fpfinal.app

import cats._
import cats.data._
import cats.implicits._
import fpfinal.common.IO
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.PersonService.PersonState
import fpfinal.service.{
  ExpenseService,
  LiveExpenseService,
  LivePersonService,
  PersonService
}

object Configuration {
  type IsValid[A] = Validated[NonEmptyChain[String], A]
  case class AppState(expenseState: ExpenseState, personState: PersonState)
  object AppState {
    implicit def eqAppState(implicit
        eqExpenseState: Eq[ExpenseState],
        eqPersonState: Eq[PersonState]
    ): Eq[AppState] =
      Eq.instance((as1, as2) =>
        as1.expenseState === as2.expenseState && as1.personState === as2.personState
      )
  }
  type Error = String
  type ErrorOr[A] = EitherT[IO, Error, A]
  type St[A] = StateT[ErrorOr, AppState, A]
  type SuccessMsg = String
  type Environment = ExpenseService
    with PersonService
    with Console
    with Controller
  val liveEnv: Environment = new LiveExpenseService
    with LivePersonService
    with LiveConsole
    with LiveController
  type AppOp[A] = ReaderT[St, Environment, A]
  def readEnv: AppOp[Environment] = ReaderT.ask[St, Environment]
}
