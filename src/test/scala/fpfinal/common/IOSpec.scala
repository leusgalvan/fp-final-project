package fpfinal.common

import cats._
import cats.implicits._
import cats.laws.discipline.MonadTests
import fpfinal.Generators
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class IOSpec
    extends AnyFunSuite
    with Configuration
    with FunSuiteDiscipline
    with Generators {

  implicit def eqIO[A: Eq]: Eq[IO[A]] =
    Eq.instance((io1, io2) => io1.run eqv io2.run)

  checkAll("Monad[IO]", MonadTests[IO].stackUnsafeMonad[Int, Int, Int])
}
