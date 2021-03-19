package fpfinal.service

import cats._
import cats.implicits._
import cats.data._
import fpfinal.model.Person

trait PersonService {
  import PersonService._
  val personService: Service

  trait Service {
    def findByName(name: String): PersonOp[Option[Person]]
    def addPerson(person: Person): PersonOp[Unit]
    def getAllPeople(): PersonOp[List[Person]]
  }
}

object PersonService {
  case class PersonState(personByName: Map[String, Person]) {
    def addPerson(person: Person): PersonState =
      copy(personByName = personByName + (person.name -> person))
  }

  object PersonState {
    implicit def eqPersonState(implicit
        eqMap: Eq[Map[String, Person]]
    ): Eq[PersonState] =
      Eq.instance((ps1, ps2) => ps1.personByName eqv ps2.personByName)
  }

  type PersonOp[A] = State[PersonState, A]
}

trait LivePersonService extends PersonService {
  import PersonService._
  override val personService: Service = new Service {
    override def findByName(name: String): PersonOp[Option[Person]] =
      State.inspect(_.personByName.get(name))

    override def addPerson(person: Person): PersonOp[Unit] =
      State.modify(_.addPerson(person))

    override def getAllPeople(): PersonOp[List[Person]] =
      State.inspect(_.personByName.values.toList)
  }
}
