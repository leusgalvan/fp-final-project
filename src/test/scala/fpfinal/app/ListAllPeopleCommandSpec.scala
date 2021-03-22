package fpfinal.app

import cats.implicits._
import fpfinal.FpFinalSpec
import fpfinal.app.Syntax._
import fpfinal.fakes.FakeEnv

class ListAllPeopleCommandSpec extends FpFinalSpec {
  test("List all people writes people name to console") {
    val env: FakeEnv = new FakeEnv {
      override var linesToRead: List[String] = Nil
    }
    forAll { (initialAppState: AppState) =>
      ListAllPeopleCommand
        .execute()
        .unsafeRunAppS(env, initialAppState) eqv Right(initialAppState)
      initialAppState.personState.personByName.keySet.forall(name =>
        env.linesWritten.contains(name)
      )
    }
  }
}
