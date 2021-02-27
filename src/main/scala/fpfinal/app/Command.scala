package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, SuccessMsg, readEnv}

import scala.collection.immutable.SortedSet
import scala.io.StdIn

sealed trait Command {
  val name: String
  def execute(): AppOp[SuccessMsg]
}

object Command {
  implicit val showCommand: Show[Command] = Show.show(_.name)
}

object AddExpenseCommand extends Command {
  val name = "Add expense"

  case class AddExpenseData(
      payer: String,
      amount: Double,
      participants: SortedSet[String]
  )

  def execute(): AppOp[SuccessMsg] = {
    implicit val ME = MonadError[AppOp, String]

    def readPayer(): AppOp[String] = {
      for {
        env <- readEnv
        name = env.console.readLine("Enter payer's name: ")
      } yield name
    }

    def readAmount(): AppOp[Double] = {
      for {
        env <- readEnv
        amount <-
          env.console
            .readDouble("Enter amount: ")
            .pure[AppOp]
            .ensure("Invalid double)")(_.nonEmpty)
      } yield amount
    }

    def readParticipants(): AppOp[SortedSet[String]] = {
      for {
        env <- readEnv
        p = env.console.readLine(
          s"Enter name of participant (or END to finish): "
        )
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

    for {
      addExpenseData <- readData()
    } yield "yay"
  }
}
