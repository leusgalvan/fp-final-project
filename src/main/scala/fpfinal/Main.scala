package fpfinal

import fpfinal.app.{App, AppState}
import fpfinal.app.Configuration._
import fpfinal.model.Person
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.PersonService.PersonState
import fpfinal.app.Syntax._

object Main {
  def main(args: Array[String]): Unit = {
    App
      .run()
      .unsafeRunApp(liveEnv, AppState.empty)
  }
}
