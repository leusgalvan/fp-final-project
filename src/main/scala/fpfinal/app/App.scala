package fpfinal.app

import fpfinal.app.Configuration.{AppOp, Environment, St, readEnv}
import cats._
import cats.implicits._
import cats.data._

object App {
  def printOptions: AppOp[Unit] = {
    def mkOptionsString(commands: List[Command]): String = {
      val header: String = "Please select an option: "
      val commandsList: List[String] = commands.zipWithIndex.map {
        case (c, i) => s"($i) ${c.show}"
      }
      (header :: commandsList).mkString("\n")
    }

    for {
      env <- readEnv
      allCommands = env.controller.getAllCommands
      options = mkOptionsString(allCommands.toList)
      _ = env.console.printLine(options)
    } yield ()
  }

  def readCommand(): AppOp[Int] =
    for {
      env <- readEnv
      option = env.console.readLine("Your option: ")
    } yield option.toInt

  def run(): AppOp[Unit] = {
    val loop: AppOp[Unit] =
      for {
        env <- readEnv
        _ <- printOptions
        commandNumber <- readCommand()
        command = env.controller.getCommandByNumber(commandNumber)
        message <- command.fold(
          MonadError[AppOp, String].raiseError[String]("Command not found")
        )(_.execute())
        _ = env.console.printLine(message)
        _ <- run
      } yield ()

    MonadError[AppOp, String].handleErrorWith(loop) { errorMessage =>
      readEnv.map(_.console.printLine(errorMessage)).flatMap(_ => run())
    }
  }
}
