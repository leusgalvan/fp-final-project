package fpfinal.service

import cats.data.State
import fpfinal.model.Person

trait PersonService {
  val personService: Service

  case class PersonState(personByName: Map[String, Person])
  type PersonOp[A] = State[PersonState, A]

  trait Service {
    def findByName(name: String): PersonOp[Person]
    def addPerson()
  }
}

trait LivePersonService extends PersonService {
  override val personService: Service = new Service {
    override def findByName(name: String): PersonOp[Person] = ???
    override def addPerson(): Unit = ???
  }
}
