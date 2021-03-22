package fpfinal.app

import cats.implicits._
import fpfinal.FpFinalSpec
import fpfinal.app.Syntax._
import fpfinal.fakes.FakeEnv
import fpfinal.model.Person

class AddPersonCommandSpec extends FpFinalSpec {
  test("Add person command reads data and adds a person to state") {
    forAll { (initialAppState: AppState) =>
      val name = "Leandro"
      val env = new FakeEnv {
        override var linesToRead: List[String] = List(name)
      }

      assert(
        AddPersonCommand
          .execute()
          .unsafeRunAppS(env, initialAppState)
          .map(_.personState.personByName.get(name))
          eqv Right(Some(Person.unsafeCreate(name)))
      )
    }
  }

  test("Trying to add an invalid person yields error") {
    forAll { (initialAppState: AppState) =>
      val name = ""
      val env = new FakeEnv {
        override var linesToRead: List[String] = List(name)
      }

      assert(
        AddPersonCommand
          .execute()
          .unsafeRunAppS(env, initialAppState)
          .isLeft
      )
    }
  }
}
