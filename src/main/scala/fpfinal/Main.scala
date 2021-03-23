package fpfinal

import fpfinal.app.Configuration._
import fpfinal.app.Syntax._
import fpfinal.app.{App, AppState}

object Main {
  def main(args: Array[String]): Unit = {
    App
      .run()
      .unsafeRunApp(liveEnv, AppState.empty)
  }
}
