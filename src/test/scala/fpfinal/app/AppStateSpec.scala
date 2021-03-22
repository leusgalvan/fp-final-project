package fpfinal.app

import cats.kernel.laws.discipline.EqTests
import fpfinal.FpFinalSpec
import cats.implicits._

class AppStateSpec extends FpFinalSpec {
  checkAll("Eq[AppState]", EqTests[AppState].eqv)
}
