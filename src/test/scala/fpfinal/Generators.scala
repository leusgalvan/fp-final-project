package fpfinal

import cats.implicits._
import fpfinal.model.{Expense, Money, PayeeDebt, PayerDebt, Person}
import org.scalacheck.{Arbitrary, Gen}

trait Generators {
  implicit val personArb: Arbitrary[Person] = Arbitrary {
    Gen.alphaStr.map(Person.unsafeCreate)
  }

  implicit val moneyArb: Arbitrary[Money] = Arbitrary {
    Gen.choose(1, Int.MaxValue).map(Money.unsafeCreate)
  }

  implicit val expenseArb: Arbitrary[Expense] = Arbitrary {
    for {
      person <- personArb.arbitrary
      money <- moneyArb.arbitrary
      participants <- Gen.nonEmptyListOf(personArb.arbitrary)
    } yield Expense.unsafeCreate(person, money, participants)
  }

  implicit val payeeDebtArb: Arbitrary[PayeeDebt] = Arbitrary {
    Gen
      .listOf(expenseArb.arbitrary)
      .map(_.map(PayeeDebt.fromExpense).combineAll)
  }

  implicit val payerDebtArb: Arbitrary[PayerDebt] = Arbitrary {
    Gen
      .listOf(expenseArb.arbitrary)
      .map(_.map(PayerDebt.fromExpense).combineAll)
  }

  implicit def functionArb[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary {
      arbA.arbitrary.map(a => (_: A) => a)
    }
}
