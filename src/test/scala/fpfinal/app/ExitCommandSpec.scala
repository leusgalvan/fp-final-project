package fpfinal.app

import cats.implicits._
import fpfinal.FpFinalSpec
import fpfinal.app.Syntax._
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
