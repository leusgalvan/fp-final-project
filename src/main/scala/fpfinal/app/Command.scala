package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, IsValid, SuccessMsg, readEnv}
import fpfinal.app.Syntax._
import fpfinal.common.Validations._
import fpfinal.model.{Expense, Money, Person}

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

    /**
      * TODO: Implement a function that reads participant names from console until the
      * user enters END to finish.
      *
      * Extra points: implement it using ME.tailRecM
      */
    def readParticipants(): AppOp[List[String]] = ???

    /**
      * TODO: Use the helper functions in common.Validations and return a validated
      * instance of AddExpenseData. The validations to perform are:
      * - payer should be a nonempty string
      * - amount should be a valid double
      * - all participants should be nonempty strings
      */
    def validateData(
        payer: String,
        amount: String,
        participants: List[String]
    ): IsValid[AddExpenseData] = ???

    def readData(): AppOp[AddExpenseData] = {
      for {
        payer <- readPayer()
        amount <- readAmount()
        participants <- readParticipants()
        validData <- validateData(payer, amount, participants).toAppOp
      } yield validData
    }

    /**
      * TODO: Implement a function that finds a person by name.
      *
      * It should be a wrapper of the PersonService analogous function,
      * handling translation between types and converting None results to errors
      * using the provided message (notice the return type is Person, and not
      * Option[Person]).
      */
    def findPerson(name: String): AppOp[Person] = {
      lazy val msg = s"Person not found: $name"
      ???
    }

    for {
      env <- readEnv
      data <- readData()
      payer <- findPerson(data.payer)
      amount <- Money.dollars(data.amount).toAppOp
      participants <- data.participants.traverse { findPerson }
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

    /**
      * TODO: Read person data from console and use it to add a person to the state.
      * Upon successful completion, return the message 'Person created successfully'.
      */
    ???
  }
}

case object ComputeDebtCommand extends Command {
  override val name: String = "Compute debt"

  override def execute(): AppOp[SuccessMsg] = {
    for {
      env <- readEnv
      payerDebt <- env.expenseService.computeDebt().toAppOp
      _ <- env.console.printLine(payerDebt.show).toAppOp
    } yield "Debt computed successfully"
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
            s"""List of people:
               |${people.map(p => s"- ${p.show}").mkString("\n")}""".stripMargin
          )
          .toAppOp
    } yield "All people listed!"
  }
}
