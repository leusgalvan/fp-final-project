package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, SuccessMsg, readEnv}
import fpfinal.model.Expense
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
      participants: SortedSet[String]
  )

  def execute(): AppOp[SuccessMsg] = {
    def readPayer(): AppOp[String] = {
      for {
        env <- readEnv
        name <- env.console.readLine("Enter payer's name: ").toAppOp
      } yield name
    }

    def readAmount(): AppOp[Double] = {
      for {
        env <- readEnv
        amountStr <- env.console.readDouble("Enter amount: ").toAppOp
        amount <- ME.fromOption(amountStr, "Invalid amount")
      } yield amount
    }

    def readParticipants(): AppOp[SortedSet[String]] = {
      for {
        env <- readEnv
        p <-
          env.console
            .readLine(
              s"Enter name of participant (or END to finish): "
            )
            .toAppOp
        ps <-
          if (p === "END") SortedSet.empty[String].pure[AppOp]
          else readParticipants()
      } yield ps + p
    }

    def readData(): AppOp[AddExpenseData] = {
      for {
        payer <- readPayer()
        amount <- readAmount()
        participants <- readParticipants()
      } yield AddExpenseData(payer, amount, participants)
    }

    // StateT[Eval, ExpenseState, A] to
    // ReaderT[
    //   StateT[Either[String, *], AppState, *],
    //   ExpenseService with PersonService with Console with Controller,
    //   A
    // ]
    for {
      env <- readEnv
      addExpenseData <- readData()
      payer = env.personService.findByName(addExpenseData.payer)
      amount = ???
      participants = ???
      expense = Expense.create(payer, amount, participants)
    } yield "yay"
  }
}
