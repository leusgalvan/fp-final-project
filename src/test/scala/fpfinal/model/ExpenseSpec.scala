package fpfinal.model

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import fpfinal.FpFinalSpec
import org.scalacheck.{Arbitrary, Gen}

class ExpenseSpec extends FpFinalSpec {
  test("create a valid expense") {
    implicit val g: Arbitrary[(Person, List[Person])] = Arbitrary {
      for {
        payer <- implicitly[Arbitrary[Person]].arbitrary
        participants <- Gen.nonEmptyListOf(
          implicitly[Arbitrary[Person]].arbitrary.filter(_ != payer)
        )
      } yield (payer, participants)
    }

    forAll { (money: Money, p: (Person, List[Person])) =>
      assert(
        Expense.create(p._1, money, p._2) === Valid(
          Expense.unsafeCreate(p._1, money, p._2)
        )
      )
    }
  }

  test("create an invalid expense with no participants") {
    forAll { (money: Money, payer: Person) =>
      assert(Expense.create(payer, money, List.empty[Person]).isInvalid)
    }
  }

  test("create an invalid expense with payer included in participants") {
    forAll { (money: Money, payer: Person, participants: List[Person]) =>
      assert(Expense.create(payer, money, payer :: participants).isInvalid)
    }
  }

  test("show an expense") {
    val e: Expense = Expense.unsafeCreate(
      Person.unsafeCreate("Martin"),
      Money.unsafeCreate(1000),
      List(Person.unsafeCreate("Susan"), Person.unsafeCreate("Bob"))
    )
    assert(
      e.show eqv "Expense[Payer=Martin,Amount=$10.00,Participants=Bob,Susan]"
    )
  }

  test("calculating amount by participant is correct up to 1 dollar tolerance") {
    forAll { (expense: Expense) =>
      val money = expense.amountByParticipant
      val centsTol = 100
      val noPeople = 1 + expense.participants.length
      assert(expense.amount.cents - money.times(noPeople).cents <= centsTol)
    }
  }

  // TODO #10: Add the missing typeclass tests for Eq
  checkAll("Eq[Expense]", EqTests[Expense].eqv)
}
