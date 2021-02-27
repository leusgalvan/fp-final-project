package fpfinal.app

import fpfinal.FakeEnv
import fpfinal.app.Configuration.liveEnv
import org.scalatest.funsuite.AnyFunSuite

class AppSpec extends AnyFunSuite {
  test("Successful command flow") {
    val fakeEnv: FakeEnv = new FakeEnv {
      override val commands: Map[Int, Command] = Map(1 -> AddExpenseCommand)
      override var linesToRead: List[String] = List("1")
    }
    App.run().run(liveEnv)
    println(fakeEnv.callsToAddExpense)
    println(fakeEnv.linesWritten)
  }
}
