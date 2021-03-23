package fpfinal.app

import cats.Eq
import cats.implicits._
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.PersonService.PersonState

case class AppState(expenseState: ExpenseState, personState: PersonState)
object AppState {
  def empty: AppState = AppState(ExpenseState.empty, PersonState.empty)

  implicit def eqAppState(implicit
      eqExpenseState: Eq[ExpenseState],
      eqPersonState: Eq[PersonState]
  ): Eq[AppState] =
    Eq.instance((as1, as2) =>
      as1.expenseState === as2.expenseState && as1.personState === as2.personState
    )
}
