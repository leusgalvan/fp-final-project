package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, readEnv}
import fpfinal.app.Syntax._

/**
 * A command line application to handle expenses among a group of people.
 *
 * It consists of a simple REPL loop which prints the available commands,
 * reads the answer from the user, and executes the selected command.
 *
 * There are five available commands:
 * - Add a person:
 *     The application will ask the user to enter the person's name, it will validate it
 *     and it will add the person to the application state.
 *
 * - Add an expense:
 *     The application will ask the user to enter the name of the payer, the amount in dollars
 *     that was paid, and the people who participated in the expense. After validating the data,
 *     it will add the expense to the application state.
 *
 * - Compute debt:
 *     The application will go through all the expenses in the state and it will calculate how much
 *     people owe each other. Once finished, it will output the results in the console, specifying
 *     for each payer, how much each person owes them.
 *
 * - List all people:
 *     The application will print a list of all the people currently in the state.
 *
 * - Exit the application
 *     The application will show a polite greeting and exit.
 *
 * Validations and other errors will be reported to the user via the console, but the application
 * will continue to run.
 */
object App {
  val ME = MonadError[AppOp, String]

  /**
   * Runs the main application loop.
   */
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
        env         <- readEnv
        allCommands  = env.controller.getAllCommands
        options      = mkOptionsString(allCommands.toList)
        _           <- env.console.printLine(options).toAppOp
      } yield ()
    }

    def readCommandNumber(): AppOp[Int] = {
      for {
        env       <- readEnv
        option    <- env.console.readLine("Your option: ").toAppOp
        cmdNumber <- ME.fromOption(option.toIntOption, "Invalid option selected")
      } yield cmdNumber
    }

    def executeCommand: AppOp[Boolean] =
      for {
        env           <- readEnv
        _             <- printOptions
        commandNumber <- readCommandNumber()
        command       <- ME.fromOption(env.controller.getCommandByNumber(commandNumber), "Command not found")
        _             <- env.console.printLine("").toAppOp
        successMsg    <- command.execute()
        _             <- env.console.printLine(s"\n$successMsg\n", Console.Success).toAppOp
      } yield command.isExit

    /**
      * TODO #30: Implement a function that executes a command and handles error by
      * printing them to console.
      *
      * The app should be able to continue normal execution afterwards.
      */
    def executeCommandWithRecovery: AppOp[Boolean] =
      ME.handleErrorWith(executeCommand) { error =>
        readEnv.flatMap { env =>
          env.console.printLine(s"\n$error\n", Console.Error).as(false).toAppOp
        }
      }

    ME.iterateUntil(executeCommandWithRecovery)(identity).void
  }
}
