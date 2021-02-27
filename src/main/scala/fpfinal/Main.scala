package fpfinal

import fpfinal.app.App
import fpfinal.app.Configuration._
import cats._
import cats.data._
import cats.implicits._
object Main {
  def main(args: Array[String]): Unit = {
    App.run().run(liveEnv).runA(AppState())
  }
}
