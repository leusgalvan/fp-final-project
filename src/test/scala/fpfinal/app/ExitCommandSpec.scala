package fpfinal.app

import fpfinal.app.Configuration.AppState
import fpfinal.FpFinalSpec
import cats.implicits._
import Syntax._
import fpfinal.fakes.FakeEnv

class ExitCommandSpec extends FpFinalSpec {
  test("Exit command does not alter state") {
    val env: FakeEnv = new FakeEnv {
      override var linesToRead: List[String] = Nil
    }
    forAll { (initialAppState: AppState) =>
      assert(
        ExitCommand
          .execute()
          .unsafeRunAppS(env, initialAppState)
          eqv Right(initialAppState)
      )
    }
  }
}
