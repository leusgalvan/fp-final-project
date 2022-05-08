package fpfinal.app

import fpfinal.common.IO

import scala.io.StdIn

/**
 * A simple implementation of a console that writes and reads
 * to and from standard input.
 */
trait Console {
  val console: Service

  trait Service {
    /**
     * Reads a line from standard input.
     *
     * @param msg a message to show in the console before reading the line (such as 'Please enter a name: ')
     */
    def readLine(msg: String): IO[String]

    /**
     * Prints a line to standard output.
     */
    def printLine(line: String): IO[Unit]
  }
}

trait LiveConsole extends Console {
  override val console: Service = new Service {
    override def readLine(msg: String): IO[String] =
      IO(StdIn.readLine(msg))

    override def printLine(line: String): IO[Unit] =
      IO(println(line))
  }
}
