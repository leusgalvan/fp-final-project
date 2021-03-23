package fpfinal.common

import cats.Order
import cats.data.NonEmptySet
import cats.data.Validated.Valid
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.SortedSet

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

  test("valid nonNegative") {
    implicit val g: Arbitrary[Double] =
      Arbitrary(Gen.choose(0, Double.MaxValue))
    forAll { (d: Double) =>
      assert(nonNegative(d) eqv Valid(d))
    }
  }

  test("invalid nonNegative") {
    implicit val g: Arbitrary[Double] = Arbitrary(Gen.negNum[Double])
    forAll { (d: Double) =>
      assert(nonNegative(d).isInvalid)
    }
  }

  test("valid nonEmptySet") {
    implicit val g: Arbitrary[List[Int]] =
      Arbitrary(Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]))
    forAll { (ints: List[Int]) =>
      assert(
        nonEmptySet(ints) eqv Valid(
          NonEmptySet.fromSetUnsafe(SortedSet.from(ints)(Order[Int].toOrdering))
        )
      )
    }
  }

  test("invalid nonEmptySet") {
    assert(nonEmptySet[Int](Nil).isInvalid)
  }

  test("valid nonEmptyString") {
    val g: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.nonEmpty)
    forAll(g) { (s: String) =>
      assert(nonEmptyString(s) eqv Valid(s))
    }
  }

  test("invalid nonEmptyString") {
    assert(nonEmptyString("").isInvalid)
  }

  test("valid allLetters") {
    forAll(Gen.alphaStr) { (s: String) =>
      assert(allLetters(s) eqv Valid(s))
    }
  }

  test("invalid allLetters") {
    val g: Gen[String] = Gen.numStr.suchThat(_.nonEmpty)
    forAll(g) { (s: String) =>
      assert(allLetters(s).isInvalid)
    }
  }

  test("valid maxLength") {
    val g: Gen[(Int, String)] = for {
      n <- Gen.choose(0, 1000)
      m <- Gen.choose(0, n)
      s <- Gen.stringOfN(m, Arbitrary.arbitrary[Char])
    } yield (n, s)

    forAll(g) {
      case (n: Int, s: String) =>
        assert(maxLength(s, n) eqv Valid(s))
    }
  }

  test("invalid maxLength") {
    val g: Gen[(Int, String)] = for {
      n <- Gen.choose(0, 1000)
      m <- Gen.choose(n + 1, n + 1000)
      s <- Gen.stringOfN(m, Arbitrary.arbitrary[Char])
    } yield (n, s)

    forAll(g) {
      case (n: Int, s: String) =>
        assert(maxLength(s, n).isInvalid)
    }
  }
}
