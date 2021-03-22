package fpfinal.app

import fpfinal.{FakeEnv, FpFinalSpec}
import Syntax._
import cats.implicits._
import fpfinal.app.Configuration.AppState

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
