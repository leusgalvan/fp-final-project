package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, IsValid, SuccessMsg, readEnv}
import fpfinal.model.{Expense, Money, Person}
import Syntax._
import fpfinal.common.Validations._

import scala.collection.immutable.SortedSet

sealed trait Command {
  val name: String
  def execute(): AppOp[SuccessMsg]
  def isExit: Boolean = false
}

object Command {
  implicit val showCommand: Show[Command] = Show.show(_.name)
}

object ExitCommand extends Command {
  override val name: String = "Exit app"

  override def isExit: Boolean = true

  override def execute(): AppOp[SuccessMsg] = "Bye :)".pure[AppOp]
}

object AddExpenseCommand extends Command {
  val name = "Add expense"

  implicit val ME = MonadError[AppOp, String]

  case class AddExpenseData(
      payer: String,
      amount: Double,
      participants: List[String]
  )

  def execute(): AppOp[SuccessMsg] = {
    def readPayer(): AppOp[String] = {
      for {
        env <- readEnv
        name <- env.console.readLine("Enter payer's name: ").toAppOp
      } yield name
    }

    def readAmount(): AppOp[String] = {
      for {
        env <- readEnv
        amount <- env.console.readLine("Enter amount: ").toAppOp
      } yield amount
    }

    def readParticipants(): AppOp[List[String]] = {
      for {
        env <- readEnv
        p <-
          env.console
            .readLine(
              s"Enter name of participant (or END to finish): "
            )
            .toAppOp
        ps <-
          if (p === "END") List.empty[String].pure[AppOp]
          else readParticipants()
      } yield if (p === "END") ps else (p :: ps)
    }

    def validateData(
        payer: String,
        amount: String,
        participants: List[String]
    ): IsValid[AddExpenseData] = {
      (
        nonEmptyString(payer),
        double(amount),
        participants.traverse(nonEmptyString)
      ).mapN { (p, a, ps) =>
        AddExpenseData(p, a, ps)
      }
    }

    def readData(): AppOp[AddExpenseData] = {
      for {
        payer <- readPayer()
        amount <- readAmount()
        participants <- readParticipants()
        validData <- validateData(payer, amount, participants).toAppOp
      } yield validData
    }

    for {
      env <- readEnv
      data <- readData()
      payer <-
        env.personService
          .findByName(data.payer)
          .toAppOp
          .flatMap(p => ME.fromOption(p, s"Payer not found: ${data.payer}"))
      amount <- Money.dollars(data.amount).toAppOp
      participants <- data.participants.traverse { p =>
        env.personService
          .findByName(p)
          .toAppOp
          .flatMap(p => ME.fromOption(p, s"Payer not found: ${p}"))
      }
      expense <- Expense.create(payer, amount, participants).toAppOp
      _ <- env.expenseService.addExpense(expense).toAppOp
    } yield "Expense created successfully"
  }
}

case object AddPersonCommand extends Command {
  override val name: String = "Add person"

  case class AddPersonData(
      name: String
  )

  override def execute(): AppOp[SuccessMsg] = {
    def validateData(
        name: String
    ): IsValid[AddPersonData] = {
      nonEmptyString(name).map(AddPersonData.apply)
    }

    def readData(): AppOp[AddPersonData] = {
      for {
        env <- readEnv
        name <- env.console.readLine("Enter name: ").toAppOp
        validData <- validateData(name).toAppOp
      } yield validData
    }

    for {
      env <- readEnv
      data <- readData()
      person <- Person.create(data.name).toAppOp
      _ <- env.personService.addPerson(person).toAppOp
    } yield "Person created successfully"
  }
}

case object ComputeDebtCommand extends Command {
  override val name: String = "Compute debt"

  override def execute(): AppOp[SuccessMsg] = {
    for {
      env <- readEnv
      payerDebt <- env.expenseService.computeDebt().toAppOp
      _ <- env.console.printLine(payerDebt.show).toAppOp
    } yield "Person created successfully"
  }
}

case object ListAllPeopleCommand extends Command {
  override val name: String = "List all people"

  override def execute(): AppOp[SuccessMsg] = {
    for {
      env <- readEnv
      people <- env.personService.getAllPeople().toAppOp
      _ <-
        env.console
          .printLine(
            s"List of people:\n\n${people.foldMap(p => s"${p.show}\n")}"
          )
          .toAppOp
    } yield "All people listed!"
  }
}
