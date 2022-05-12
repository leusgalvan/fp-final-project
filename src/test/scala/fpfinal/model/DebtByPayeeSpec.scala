package fpfinal.model

import cats.data.NonEmptySet
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import fpfinal.FpFinalSpec

import scala.collection.immutable.SortedSet

class DebtByPayeeSpec extends FpFinalSpec {
  test("allPayees should return the payees") {
    val personA = Person.unsafeCreate("A")
    val personB = Person.unsafeCreate("B")
    val moneyA = Money.unsafeCreate(1000)
    val moneyB = Money.unsafeCreate(2000)
    val d = DebtByPayee.unsafeCreate(
      Map(
        personA -> moneyA,
        personB -> moneyB
      )
    )
    assert(d.allPayees().toSet eqv Set(personA, personB))
  }

  test("getting debt for an existing payee should return it") {
    val personA = Person.unsafeCreate("A")
    val personB = Person.unsafeCreate("B")
    val moneyA = Money.unsafeCreate(1000)
    val moneyB = Money.unsafeCreate(2000)
    val d = DebtByPayee.unsafeCreate(
      Map(
        personA -> moneyA,
        personB -> moneyB
      )
    )
    val debtForA = d.debtForPayee(personA)
    assert(debtForA eqv Some(moneyA))
  }

  test("getting debt for a non-existing payee should return None") {
    val personA = Person.unsafeCreate("A")
    val personB = Person.unsafeCreate("B")
    val moneyA = Money.unsafeCreate(1000)
    val moneyB = Money.unsafeCreate(2000)
    val d = DebtByPayee.unsafeCreate(
      Map(
        personA -> moneyA,
        personB -> moneyB
      )
    )
    val debtForC = d.debtForPayee(Person.unsafeCreate("C"))
    assert(debtForC eqv None)
  }

  test("payees are the participants from the expense") {
    forAll { (expense: Expense) =>
      assert(
        DebtByPayee
          .fromExpense(expense)
          .allPayees()
          .toSet eqv expense.participants.toList.toSet
      )
    }
  }

  test("combining yields the union of the payees") {
    forAll { (e1: Expense, e2: Expense) =>
      val lhs = NonEmptySet.fromSetUnsafe(
        SortedSet.from(
          (DebtByPayee.fromExpense(e1) |+| DebtByPayee.fromExpense(e2))
            .allPayees()
        )
      )
      val rhs = e1.participants |+| e2.participants
      assert(lhs eqv rhs)
    }
  }

  test("combining sums the debts of payees") {
    forAll { (e1: Expense, e2: Expense) =>
      val d1 = DebtByPayee.fromExpense(e1)
      val d2 = DebtByPayee.fromExpense(e2)
      val d = d1 |+| d2
      assert(d.allPayees().forall { p =>
        d.debtForPayee(p) eqv d1.debtForPayee(p) <+> d2.debtForPayee(p)
      })
    }
  }

  test("every participant of the expense get the same debt") {
    forAll { (expense: Expense) =>
      val debtByPayee = DebtByPayee.fromExpense(expense)
      val firstPayee = expense.participants.head
      val firstPayeeDebt = debtByPayee.debtForPayee(firstPayee)
      assert(
        expense.participants.forall(p =>
          debtByPayee.debtForPayee(p) eqv firstPayeeDebt
        )
      )
    }
  }

  // TODO #17: Add typeclass tests for Eq and Monoid
  checkAll("Eq[DebtByPayee]", EqTests[DebtByPayee].eqv)
  checkAll("Monoid[DebtByPayee]", MonoidTests[DebtByPayee].monoid)
}
