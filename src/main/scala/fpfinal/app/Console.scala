package fpfinal.app

import cats._
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import fpfinal.app.Configuration.IsValid

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

trait Console {
  val console: Service

  trait Service {
    def readLine(msg: String): String
    def printLine(line: String): Unit
    def readDouble(msg: String): Option[Double]
  }
}

trait LiveConsole extends Console {
  override val console: Service = new Service {
    override def readLine(msg: String): String = StdIn.readLine(msg)
    override def printLine(line: String): Unit = println(line)
    override def readDouble(msg: String): Option[Double] = {
      Try(readLine(msg).toDouble).toOption
    }
  }
}
