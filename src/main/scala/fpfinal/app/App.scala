package fpfinal.app

import fpfinal.app.Configuration.{AppOp, Environment}
import cats._
import cats.implicits._
import cats.data._

object App {
  def run(): AppOp[Unit] =
    Applicative[AppOp].unit
}
