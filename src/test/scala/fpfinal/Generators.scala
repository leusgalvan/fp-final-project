package fpfinal

import cats.data.{NonEmptyChain, State}
import cats.data.Validated.Valid
import cats.implicits._
import fpfinal.app.AppState
import fpfinal.app.Configuration.IsValid
import fpfinal.common.IO
import fpfinal.common.IO.{Done, FlatMap, More}
import fpfinal.model.{DebtByPayee, DebtByPayer, Expense, Money, Person}
import fpfinal.service.ExpenseService.{ExpenseOp, ExpenseState}
import fpfinal.service.PersonService.{PersonOp, PersonState}
import org.scalacheck.{Arbitrary, Gen}

trait Generators {
  implicit val personArb: Arbitrary[Person] = Arbitrary {
    for {
      n <- Gen.choose(0, 32)
      name <- Gen.stringOfN(n, Gen.alphaChar)
    } yield Person.unsafeCreate(name)
  }

  implicit val moneyArb: Arbitrary[Money] = Arbitrary {
    Gen.choose(1, 1e9.toInt).map(Money.unsafeCreate)
  }

  implicit def expenseArb(implicit
      arbPerson: Arbitrary[Person],
      arbMoney: Arbitrary[Money]
  ): Arbitrary[Expense] =
    Arbitrary {
      for {
        person <- arbPerson.arbitrary
        money <- arbMoney.arbitrary
        participants <- Gen.nonEmptyListOf(arbPerson.arbitrary)
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

  implicit def personStateArb(implicit
      personArb: Arbitrary[Person]
  ): Arbitrary[PersonState] =
    Arbitrary {
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

  implicit def expenseStateArb(implicit
      expenseArb: Arbitrary[Expense]
  ): Arbitrary[ExpenseState] =
    Arbitrary {
      Gen.listOf(expenseArb.arbitrary).map(ExpenseState.apply)
    }

  implicit def appStateArb(implicit
      arbPersonState: Arbitrary[PersonState],
      arbExpenseState: Arbitrary[ExpenseState]
  ): Arbitrary[AppState] =
    Arbitrary {
      for {
        ps <- arbPersonState.arbitrary
        es <- arbExpenseState.arbitrary
      } yield AppState(es, ps)
    }

  implicit def personOpArb[A](implicit
      arbA: Arbitrary[A],
      arbPersonState: Arbitrary[PersonState]
  ): Arbitrary[PersonOp[A]] = {
    Arbitrary(for {
      a <- arbA.arbitrary
      ps <- arbPersonState.arbitrary
    } yield State((_: PersonState) => (ps, a)))
  }

  implicit def isValidArb[A](implicit
      arbA: Arbitrary[A]
  ): Arbitrary[IsValid[A]] = {
    val validGen: Gen[IsValid[A]] = arbA.arbitrary.map(_.validNec[String])
    val invalidGen: Gen[IsValid[A]] = Gen
      .nonEmptyListOf(Arbitrary.arbitrary[String])
      .map(xs => NonEmptyChain.fromSeq(xs).get.invalid[A])
    Arbitrary(Gen.oneOf(validGen, invalidGen))
  }

  implicit def expenseOpGen[A](implicit
      arbA: Arbitrary[A],
      expenseStateArb: Arbitrary[ExpenseState]
  ): Arbitrary[ExpenseOp[A]] =
    Arbitrary {
      for {
        a <- arbA.arbitrary
        es <- expenseStateArb.arbitrary
      } yield State((_: ExpenseState) => (es, a))
    }
}
