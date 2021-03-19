package fpfinal.common

import cats.data.Validated.Valid
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValidationsSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  import Validations._

  test("valid doubles") {
    forAll { (d: Double) =>
      assert(double(d.toString) eqv Valid(d))
    }
  }

  test("invalid doubles") {
    implicit val g: Arbitrary[String] = Arbitrary(Gen.alphaStr)
    forAll { (s: String) =>
      assert(double(s).isInvalid)
    }
  }
}
