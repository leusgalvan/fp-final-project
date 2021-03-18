package fpfinal.service

import cats.data.State
import fpfinal.model.Person

trait PersonService {
  import PersonService._
  val personService: Service

  trait Service {
    def findByName(name: String): PersonOp[Option[Person]]
    def addPerson()
  }
}

object PersonService {
  case class PersonState(personByName: Map[String, Person])
  type PersonOp[A] = State[PersonState, A]
}

trait LivePersonService extends PersonService {
  import PersonService._
  override val personService: Service = new Service {
    override def findByName(name: String): PersonOp[Option[Person]] =
      State.inspect(_.personByName.get(name))

    override def addPerson(): Unit = ???
  }
}
