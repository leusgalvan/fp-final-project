package fpfinal

import cats.implicits._
import fpfinal.common.IO
import fpfinal.common.IO.{Done, FlatMap, More}
import fpfinal.model.{DebtByPayee, DebtByPayer, Expense, Money, Person}
import fpfinal.service.PersonService.PersonState
import org.scalacheck.{Arbitrary, Gen}

trait Generators {
  implicit val personArb: Arbitrary[Person] = Arbitrary {
    Gen.alphaStr.map(Person.unsafeCreate)
  }

  implicit val moneyArb: Arbitrary[Money] = Arbitrary {
    Gen.choose(1, 1e9.toInt).map(Money.unsafeCreate)
  }

  implicit val expenseArb: Arbitrary[Expense] = Arbitrary {
    for {
      person <- personArb.arbitrary
      money <- moneyArb.arbitrary
      participants <- Gen.nonEmptyListOf(personArb.arbitrary)
    } yield Expense.unsafeCreate(person, money, participants)
  }

  implicit val payeeDebtArb: Arbitrary[DebtByPayee] = Arbitrary {
    Gen
      .listOf(expenseArb.arbitrary)
      .map(_.map(DebtByPayee.fromExpense).combineAll)
  }

  implicit val payerDebtArb: Arbitrary[DebtByPayer] = Arbitrary {
    Gen
      .listOf(expenseArb.arbitrary)
      .map(_.map(DebtByPayer.fromExpense).combineAll)
  }

  implicit def functionArb[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary {
      arbA.arbitrary.map(a => (_: A) => a)
    }

  implicit val personStateArb: Arbitrary[PersonState] = Arbitrary {
    Gen
      .mapOf[String, Person](personArb.arbitrary.map(p => (p.name, p)))
      .map(PersonState.apply)
  }

  implicit def ioArb[A](implicit arbA: Arbitrary[A]): Arbitrary[IO[A]] = {
    val doneGen: Gen[Done[A]] = arbA.arbitrary.map(Done.apply)
    val moreGen: Gen[More[A]] = arbA.arbitrary.map(a => More(() => Done(a)))
    val flatMapGen: Gen[FlatMap[A, A]] =
      for {
        fa <- Gen.oneOf(doneGen, moreGen)
        ta <- Gen.oneOf(doneGen, moreGen)
      } yield FlatMap(ta, (_: A) => fa)

    Arbitrary(Gen.oneOf(doneGen, moreGen, flatMapGen))
  }
}
