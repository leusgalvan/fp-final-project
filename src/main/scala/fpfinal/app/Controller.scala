package fpfinal.app

import scala.util.Try

trait Controller {
  val controller: Service

  trait Service {
    def getCommandByNumber(number: Int): Option[Command]
  }
}

trait LiveController extends Controller {
  val allCommands: Array[Command] = Array(AddExpenseCommand)

  override val controller = new Service {
    override def getCommandByNumber(number: Int): Option[Command] =
      Try(allCommands(number - 1)).toOption
  }
}
