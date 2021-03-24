package fpfinal.model

import cats._
import cats.data._
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._

import scala.collection.immutable.SortedSet

class Expense private (
    val payer: Person,
    val amount: Money,
    val participants: NonEmptySet[Person]
) {
  def amountByParticipant: Money =
    amount
      .divideBy(participants.length + 1)
      .get // safe get because divisor is never 0
  override def toString: String =
    Show[Expense].show(this)
}

object Expense {
  def unsafeCreate(
      payer: Person,
      amount: Money,
      participants: List[Person]
  ): Expense =
    new Expense(
      payer,
      amount,
      NonEmptySet.fromSetUnsafe(SortedSet.from(participants))
    )

  def create(
      payer: Person,
      amount: Money,
      participants: List[Person]
  )(implicit eqPerson: Eq[Person]): IsValid[Expense] = {
    (
      nonEmptySet(participants),
      Validated.condNec(
        Foldable[List].forall(participants)(_ neqv payer),
        payer,
        "payer cannot be included in participants"
      )
    ).mapN { (ps, p) =>
      new Expense(p, amount, ps)
    }
  }

  implicit def eqExpense(implicit
      eqPerson: Eq[Person],
      eqMoney: Eq[Money],
      eqParticipants: Eq[NonEmptySet[Person]]
  ): Eq[Expense] =
    Eq.instance((e1, e2) =>
      e1.payer === e2.payer && e1.amount === e2.amount && e1.participants === e2.participants
    )

  implicit val showExpense: Show[Expense] = Show.show { e =>
    s"Expense[Payer=${e.payer.show},Amount=${e.amount.show},Participants=${e.participants.mkString_(",")}]"
  }
}
