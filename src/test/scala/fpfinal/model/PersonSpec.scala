package fpfinal.model

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, OrderTests}
import fpfinal.FpFinalSpec
import org.scalacheck.Gen

class PersonSpec extends FpFinalSpec {
  test("create a valid person") {
    val g: Gen[String] = for {
      n <- Gen.choose(1, 32)
      s <- Gen.stringOfN(n, Gen.alphaChar)
    } yield s

    forAll(g) { (name: String) =>
      assert(Person.create(name) eqv Valid(Person.unsafeCreate(name)))
    }
  }

  test("create invalid person with empty name") {
    assert(Person.create("").isInvalid)
  }

  test("create invalid person with long name") {
    val g: Gen[String] = for {
      n <- Gen.choose(33, 1000)
      s <- Gen.stringOfN(n, Gen.alphaChar)
    } yield s

    forAll(g) { (name: String) =>
      assert(Person.create(name).isInvalid)
    }
  }

  checkAll("Eq[Person]", EqTests[Person].eqv)
  checkAll("Ord[Person]", OrderTests[Person].order)
}
