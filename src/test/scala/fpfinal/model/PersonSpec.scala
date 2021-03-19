package fpfinal.model

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class PersonSpec extends ModelSpec {
  test("create a valid person") {
    implicit val stringArb: Arbitrary[String] = Arbitrary(Gen.alphaStr)
    forAll { (name: String) =>
      Person.create(name) eqv Valid(Person.unsafeCreate(name))
    }
  }

  test("create invalid person with empty name") {
    Person.create("").isInvalid
  }

  checkAll("Eq[Person]", EqTests[Person].eqv)
}
