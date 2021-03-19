import cats._
import cats.implicits._

object Outer {

  case class MyClass(a: Int)

  object MyClass {
    implicit val eqM = Eq.instance[MyClass]((x, y) => x.a == y.a)
  }

}
import Outer._
MyClass(1) eqv MyClass(2)