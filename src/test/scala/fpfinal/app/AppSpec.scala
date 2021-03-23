package fpfinal.app

import fpfinal.fakes.FakeEnv
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.PersonService.PersonState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AppSpec extends AnyFunSuite with Matchers {
  test("Successful command flow") {
    val fakeEnv: FakeEnv = new FakeEnv {
      override var linesToRead: List[String] = List(
        "0", // The command number (add person)
        "Leandro", // The name of the person
        "1", // The command number (add expense)
        "Leandro", // The payer
        "2000.00", // The amount
        "Martin", // The first participant
        "Susan", // The second participant
        "END", // No more participants
        "4" // The command number (exit)
      )
    }

    App
      .run()
      .run(fakeEnv)
      .run(AppState(ExpenseState(Nil), PersonState(Map.empty)))
      .value
      .run
      .isRight
  }

  test("Invalid flow example") {
    val fakeEnv: FakeEnv = new FakeEnv {
      override var linesToRead: List[String] = List(
        "0", // The command number (add person)
        "Leandro", // The name of the person
        "1", // The command number (add expense)
        "Masi", // The payer (invalid because it does not exist)
        "2000.00", // The amount
        "Martin", // The first participant
        "Susan", // The second participant
        "END", // No more participants
        "3" // List all people
      )
    }

    App
      .run()
      .run(fakeEnv)
      .run(AppState(ExpenseState(Nil), PersonState(Map.empty)))
      .value
      .run
      .isRight
  }
}
