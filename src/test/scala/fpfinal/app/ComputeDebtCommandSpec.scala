package fpfinal.app

import fpfinal.FpFinalSpec
import fpfinal.app.Syntax._
import fpfinal.fakes.FakeEnv

class ComputeDebtCommandSpec extends FpFinalSpec {
  test("compute debt does not crash") {
    val env: FakeEnv = new FakeEnv {
      override var linesToRead: List[String] = Nil
    }
    forAll { (initialState: AppState) =>
      ComputeDebtCommand.execute().unsafeRunApp(env, initialState).isRight
    }

  }
}
