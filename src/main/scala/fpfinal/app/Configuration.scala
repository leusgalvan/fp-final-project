package fpfinal.app

import cats.data._
import fpfinal.common.IO
import fpfinal.service.{
  ExpenseService,
  LiveExpenseService,
  LivePersonService,
  PersonService
}

/**
 * Common types and values used throughout the application.
 */
object Configuration {
  type IsValid[A] = Validated[NonEmptyChain[String], A]
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
