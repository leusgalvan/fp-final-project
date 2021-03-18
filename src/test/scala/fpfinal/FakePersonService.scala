package fpfinal

import cats._
import cats.implicits._
import fpfinal.model.Person
import fpfinal.service.PersonService
import fpfinal.service.PersonService.PersonOp

trait FakePersonService extends PersonService {
  var callsToAddPerson = 0
  var peopleSearched = 0

  override val personService: Service = new Service {
    override def findByName(name: String): PersonOp[Option[Person]] = {
      peopleSearched += 1
      Option(Person(name)).pure[PersonOp]
    }

    override def addPerson(): Unit = {
      callsToAddPerson += 1
      ()
    }
  }
}
