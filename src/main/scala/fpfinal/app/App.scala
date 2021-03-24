package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, readEnv}
import fpfinal.app.Syntax._

object App {
  val ME = MonadError[AppOp, String]

  def run(): AppOp[Unit] = {
    def printOptions: AppOp[Unit] = {
      def mkOptionsString(commands: List[Command]): String = {
        val header: String = "Please select an option: \n"
        val commandsList: List[String] = commands.zipWithIndex.map {
          case (c, i) => s"($i) ${c.show}"
        }
        (header :: commandsList).mkString("\n") + "\n"
      }

      for {
        env <- readEnv
        allCommands = env.controller.getAllCommands
        options = mkOptionsString(allCommands.toList)
        _ <- env.console.printLine(options).toAppOp
      } yield ()
    }

    def readCommandNumber(): AppOp[Int] = {
      for {
        env <- readEnv
        option <- env.console.readLine("Your option: ").toAppOp
        cmdNumber <-
          ME.fromOption(option.toIntOption, "Invalid option selected")
      } yield cmdNumber
    }

    def executeCommand: AppOp[Boolean] =
      for {
        env <- readEnv
        _ <- printOptions
        commandNumber <- readCommandNumber()
        command <- ME.fromOption(
          env.controller.getCommandByNumber(commandNumber),
          "Command not found"
        )
        _ <- env.console.printLine("").toAppOp
        successMsg <- command.execute()
        _ <- env.console.printLine(s"\n$successMsg\n").toAppOp
      } yield command.isExit

    /**
      * TODO: Implement a function that executes a command and handles error by
      * printing them to console.
      *
      * The app should be able to continue normal execution afterwards.
      */
    def executeCommandWithRecovery: AppOp[Boolean] = ???

    ME.iterateUntil(executeCommandWithRecovery)(identity).void
  }
}
