package fpfinal.app

import fpfinal.app.Configuration.AppState
import fpfinal.{FakeEnv, FpFinalSpec}
import cats.implicits._
import Syntax._
import fpfinal.model.Person

import scala.Right

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
