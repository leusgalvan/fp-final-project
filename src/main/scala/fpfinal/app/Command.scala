package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, IsValid, SuccessMsg, readEnv}
import fpfinal.model.{Expense, Money}
import Syntax._

import scala.collection.immutable.SortedSet

sealed trait Command {
  val name: String
  def execute(): AppOp[SuccessMsg]
}

object Command {
  implicit val showCommand: Show[Command] = Show.show(_.name)
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
      } yield p :: ps
    }

    def validateData(
        payer: String,
        amount: String,
        participants: List[String]
    ): IsValid[AddExpenseData] = {
      ???
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
      payer <- env.personService.findByName(data.payer).toAppOp
      amount <- Money.dollars(data.amount).toAppOp
      participants <-
        data.participants.traverse(env.personService.findByName).toAppOp
      expense <- Expense.create(payer, amount, participants).toAppOp
      _ <- env.expenseService.addExpense(expense).toAppOp
    } yield "Expense successfully created"
  }
}
