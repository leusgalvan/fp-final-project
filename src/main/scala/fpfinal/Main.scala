package fpfinal

import fpfinal.app.App
import fpfinal.app.Configuration._
import fpfinal.model.Person
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.PersonService.PersonState

object Main {
  def main(args: Array[String]): Unit = {
    App
      .run()
      .run(liveEnv)
      .runA(AppState(ExpenseState(Nil), PersonState(Map.empty[String, Person])))
      .value
      .run
  }
}
