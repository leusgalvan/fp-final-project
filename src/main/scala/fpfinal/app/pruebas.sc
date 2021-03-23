import cats._
import cats.implicits._
import fpfinal.app.AppState
import fpfinal.app.Configuration._
import fpfinal.model._
import fpfinal.service.ExpenseService._
import fpfinal.service.PersonService.PersonState

val ME = MonadError[AppOp, String]

val money = Money.unsafeCreate(1000)
val leus = Person.unsafeCreate("leus")
val masi = Person.unsafeCreate("masi")
val expense = Expense.unsafeCreate(leus, money, List(masi))
val initialState = AppState(
  ExpenseState(List(expense)),
  PersonState(Map("leus" -> leus, "masi" -> masi))
)

ME.re(ME.raiseError[String]("caca"))(s => s.pure[AppOp])
  .run(liveEnv).run(initialState).value.run