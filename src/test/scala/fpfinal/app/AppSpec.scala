package fpfinal.app

import fpfinal.FakeEnv
import org.scalatest.funsuite.AnyFunSuite

class AppSpec extends AnyFunSuite {
  test("Successful command flow") {
    val fakeEnv: FakeEnv = new FakeEnv {
      override val commands: Map[Int, Command] = Map(1 -> AddExpenseCommand)
      override var linesToRead: List[String] = List("1")
    }
    App.run().run(fakeEnv)
    println(fakeEnv.callsToAddExpense)
    println(fakeEnv.linesWritten)
  }
}
