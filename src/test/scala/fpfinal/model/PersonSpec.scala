package fpfinal.model

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import fpfinal.FpFinalSpec
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class PersonSpec extends FpFinalSpec {
  test("create a valid person") {
    implicit val stringArb: Arbitrary[String] =
      Arbitrary(Gen.alphaStr.suchThat(_.nonEmpty))

    forAll { (name: String) =>
      assert(Person.create(name) eqv Valid(Person.unsafeCreate(name)))
    }
  }

  test("create invalid person with empty name") {
    assert(Person.create("").isInvalid)
  }

  checkAll("Eq[Person]", EqTests[Person].eqv)
}
