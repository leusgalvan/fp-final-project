package fpfinal.model

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, OrderTests}
import fpfinal.FpFinalSpec
import org.scalacheck.Gen

class PersonSpec extends FpFinalSpec {
  test("create a valid person") {
    val g: Gen[String] = Gen.alphaStr.suchThat(s => s.nonEmpty && s.length < 32)

    forAll(g) { (name: String) =>
      assert(Person.create(name) eqv Valid(Person.unsafeCreate(name)))
    }
  }

  test("create invalid person with empty name") {
    assert(Person.create("").isInvalid)
  }

  checkAll("Eq[Person]", EqTests[Person].eqv)
  checkAll("Ord[Person]", OrderTests[Person].order)
}
