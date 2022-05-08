package fpfinal.app

import scala.util.Try

/**
 * A class which provides access to the commands in the application.
 */
trait Controller {
  val controller: Service

  trait Service {
    /**
     * @return an array with all the commands in the application
     */
    def getAllCommands: Array[Command]

    /**
     * @param number the number of command to retrieve (starting from 0)
     * @return the command with the given number or None if it was not found
     */
    def getCommandByNumber(number: Int): Option[Command]
  }
}

/**
 * A controller implementation which just stores all the commands in an array instance field.
 */
trait LiveController extends Controller {
  private val allCommands: Array[Command] =
    Array(
      AddPersonCommand,
      AddExpenseCommand,
      ComputeDebtCommand,
      ListAllPeopleCommand,
      ExitCommand
    )

  override val controller = new Service {
    override def getCommandByNumber(number: Int): Option[Command] =
      Try(allCommands(number)).toOption

    override def getAllCommands: Array[Command] = allCommands
  }
}
