package fpfinal

import fpfinal.app.{Command, Controller}

trait FakeController extends Controller {
  val commands: Map[Int, Command]

  override val controller: Service = new Service {
    override def getCommandByNumber(number: Int): Option[Command] =
      commands.get(number)

    override def getAllCommands: Array[Command] = commands.values.toArray
  }
}
