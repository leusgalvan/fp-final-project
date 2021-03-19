package fpfinal.model

import cats.data.Validated.Valid
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class MoneySpec extends ModelSpec {
  test("unsafe create") {
    forAll { (c: Int) =>
      Money.unsafeCreate(c).cents eqv c
    }
  }

  test("create a valid money") {
    implicit val positiveArb: Arbitrary[Double] =
      Arbitrary(Gen.choose(0, Int.MaxValue))
    forAll { (dls: Double) =>
      Money.dollars(dls) === Valid(Money.unsafeCreate((dls * 100).toInt))
    }
  }

  test("create invalid money with negative amount") {
    implicit val positiveArb: Arbitrary[Double] =
      Arbitrary(Gen.choose(Int.MinValue, -1))
    forAll { (dls: Double) =>
      Money.dollars(dls).isInvalid
    }
  }

  test("dollars and cents relation") {
    forAll { (money: Money) =>
      (money.cents * 100.0 - money.dollars) < 0.001
    }
  }

  test("divide by 0 yields None") {
    forAll { (money: Money) =>
      money.divideBy(0) eqv None
    }
  }

  test("cents of a division") {
    implicit val posInt: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])
    forAll { (money: Money, n: Int) =>
      money.divideBy(n).map(_.cents) eqv Some(money.cents / n)
    }
  }
  checkAll("Eq[Money]", EqTests[Money].eqv)
  checkAll("Monoid[Money]", MonoidTests[Money].monoid)
}
