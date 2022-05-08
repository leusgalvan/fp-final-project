package fpfinal.app

import cats.Eq
import cats.implicits._
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.PersonService.PersonState

/**
 * The application state is the combination of the ExpenseState and the PersonState.
 *
 * That is, it contains information about what expenses have been added and
 * what people have been added so far.
 */
case class AppState(expenseState: ExpenseState, personState: PersonState)

object AppState {
  /**
   * An application state without expenses nor people added.
   */
  def empty: AppState = AppState(ExpenseState.empty, PersonState.empty)

  implicit def eqAppState(implicit
      eqExpenseState: Eq[ExpenseState],
      eqPersonState: Eq[PersonState]
  ): Eq[AppState] =
    Eq.instance((as1, as2) =>
      as1.expenseState === as2.expenseState && as1.personState === as2.personState
    )
}
