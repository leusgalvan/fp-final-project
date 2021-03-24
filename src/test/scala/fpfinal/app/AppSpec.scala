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
        "0", // The command number (add person)
        "Martin", // The name of the person
        "1", // The command number (add expense)
        "Masi", // The payer (does not exist)
        "2000", // The amount
        "Martin", // First participant
        "END", // No more participants
        "1", // The command number (add expense)
        "Martin", // The payer
        "2000", // The amount
        "Leandro", // First participant
        "END", // No more participants
        "4" // Exit
      )
    }

    App
      .run()
      .run(fakeEnv)
      .run(AppState(ExpenseState(Nil), PersonState(Map.empty)))
      .value
      .run
      .isRight

    assert(fakeEnv.linesWritten.mkString(" ").contains("Person not found"))
  }
}
