package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, SuccessMsg}

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
  def execute(): AppOp[SuccessMsg] = {
    def readLine(msg: String): AppOp[String] =
      Applicative[AppOp].pure {
        StdIn.readLine(msg)
      }

    def readParticipants(): AppOp[SortedSet[String]] = {
      for {
        p <- readLine(s"Enter name of participant (or END to finish): ")
        ps <-
          if (p === "END") SortedSet.empty[String].pure[AppOp]
          else readParticipants()
      } yield ps + p
    }

    for {
      payer <- readLine("Enter payer's name: ")
      amount <- readLine("Enter amount: ")
      participants <- readParticipants()
    } yield "yay"
  }
}
