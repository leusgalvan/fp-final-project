package fpfinal.services

import fpfinal.Generators
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

trait ServiceSpec
    extends AnyFunSuite
    with Matchers
    with Generators
    with Configuration
    with FunSuiteDiscipline
