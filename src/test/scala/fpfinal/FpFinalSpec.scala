package fpfinal

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

trait FpFinalSpec
    extends AnyFunSuite
    with Generators
    with Configuration
    with FunSuiteDiscipline
    with ScalaCheckDrivenPropertyChecks
