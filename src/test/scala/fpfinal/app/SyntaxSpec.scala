package fpfinal.app

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import fpfinal.FpFinalSpec
import fpfinal.app.Configuration.{IsValid, liveEnv}
import fpfinal.app.Syntax._
import fpfinal.common.IO
import fpfinal.service.ExpenseService.ExpenseOp
import fpfinal.service.PersonService.PersonOp

class SyntaxSpec extends FpFinalSpec {
  test("IO toAppOp") {
    forAll { (io: IO[Int], appState: AppState) =>
      assert(
        io.toAppOp.run(liveEnv).run(appState).value.run eqv Right(
          (appState, io.run)
        )
      )
    }
  }

  test("PersonOp toAppOp") {
    forAll { (personOp: PersonOp[Int], appState: AppState) =>
      val (ps, n) = personOp.run(appState.personState).value
      assert(
        personOp.toAppOp.run(liveEnv).run(appState).value.run eqv Right(
          (appState.copy(personState = ps), n)
        )
      )
    }
  }

  test("IsValid toAppOp") {
    forAll { (isValidInt: IsValid[Int], appState: AppState) =>
      val result = isValidInt.toAppOp.run(liveEnv).run(appState).value.run
      isValidInt match {
        case Valid(value)    => assert(result eqv Right((appState, value)))
        case Invalid(errors) => assert(result.isLeft)
      }
    }
  }

  test("ExpenseOp toAppOp") {
    forAll { (expenseOp: ExpenseOp[Int], appState: AppState) =>
      val (es, n) = expenseOp.run(appState.expenseState).value
      assert(
        expenseOp.toAppOp.run(liveEnv).run(appState).value.run eqv Right(
          (appState.copy(expenseState = es), n)
        )
      )
    }
  }
}
