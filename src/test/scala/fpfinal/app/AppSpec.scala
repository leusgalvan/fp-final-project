package fpfinal.app

import fpfinal.FakeEnv
import fpfinal.app.Configuration.{AppState, liveEnv}
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.PersonService.PersonState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AppSpec extends AnyFunSuite with Matchers {
  test("Successful command flow") {
    val fakeEnv: FakeEnv = new FakeEnv {
      override val commands: Map[Int, Command] =
        Map(0 -> AddPersonCommand, 1 -> AddExpenseCommand, 2 -> ExitCommand)
      override var linesToRead: List[String] = List(
        "0", // The command number (add expense)
        "Leandro", // The name of the person
        "1", // The command number (add expense)
        "Leandro", // The payer
        "2000.00", // The amount
        "Martin", // The first participant
        "Susan", // The second participant
        "END", // No more participants
        "2" // The command number (exit)
      )
    }

    val x = App
      .run()
      .run(fakeEnv)
      .run(AppState(ExpenseState(Nil), PersonState(Map.empty)))
      .value
      .run

    println(x)

    println(fakeEnv.linesWritten.mkString("\n"))
    fakeEnv.peopleSearched shouldBe 3
    fakeEnv.callsToAddExpense shouldBe 1
  }
}
