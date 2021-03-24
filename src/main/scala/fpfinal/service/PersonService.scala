package fpfinal.service

import cats._
import cats.data._
import cats.implicits._
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
    def empty: PersonState = PersonState(Map.empty)

    implicit def eqPersonState(implicit
        eqMap: Eq[Map[String, Person]]
    ): Eq[PersonState] =
      Eq.instance((ps1, ps2) => ps1.personByName eqv ps2.personByName)
  }

  type PersonOp[A] = State[PersonState, A]
}

/**
  * Implement a LivePersonService with an implementation for PersonService.
  *
  * findByName returns the person in the state with the given name,
  * or None if it doesn't find it.
  *
  * addPerson adds a Person to the state, returning Unit.
  *
  * getAllPeople returns a list with all the people in the state.
  */
// trait LivePersonService...
