package fpfinal.app

import fpfinal.{FakeEnv, FpFinalSpec}
import fpfinal.app.Configuration.AppState
import cats.implicits._
import Syntax._

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
