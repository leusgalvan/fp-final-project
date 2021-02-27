package fpfinal.model

import fpfinal.app.Configuration.IsValid

case class Person private (name: String)

object Person {
  def createPerson(name: String): IsValid[Person] = ???
}
