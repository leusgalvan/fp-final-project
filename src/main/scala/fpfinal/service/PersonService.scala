package fpfinal.service

import cats._
import cats.data._
import cats.implicits._
import fpfinal.model.Person
import fpfinal.service.PersonService.PersonOp

/**
 * A trait for adding, storing and then finding people.
 *
 * It uses the State monad to keep track of the people that the user of the application
 * has added so far.
 */
trait PersonService {
  import PersonService._

  val personService: Service

  trait Service {
    /**
     * Finds a person with the given name, returning if it finds it in the state,
     * or yielding None otherwise.
     */
    def findByName(name: String): PersonOp[Option[Person]]

    /**
     * Adds a person to the state.
     */
    def addPerson(person: Person): PersonOp[Unit]

    /**
     * Returns all the people in the state.
     */
    def getAllPeople(): PersonOp[List[Person]]
  }
}

object PersonService {
  /**
   * Represents a state containing people indexed by name.
   *
   * @param personByName a map with names as keys and the associated Person instance as values.
   */
  case class PersonState(personByName: Map[String, Person]) {
    /**
     * @return a new state with the given person added
     */
    def addPerson(person: Person): PersonState =
      copy(personByName = personByName + (person.name -> person))
  }

  object PersonState {
    /**
     * A state with no people.
     */
    def empty: PersonState = PersonState(Map.empty)

    implicit def eqPersonState(implicit
        eqMap: Eq[Map[String, Person]]
    ): Eq[PersonState] =
      Eq.instance((ps1, ps2) => ps1.personByName eqv ps2.personByName)
  }

  type PersonOp[A] = State[PersonState, A]
}

/**
  * TODO #22: Provide a LivePersonService with an implementation for PersonService.
  */
trait LivePersonService extends PersonService {
  override val personService: Service = new Service {
    /**
     * Finds a person with the given name, returning it if it finds it in the state,
     * or yielding None otherwise.
     */
    override def findByName(name: String): PersonOp[Option[Person]] =
      State.inspect(_.personByName.get(name))

    /**
     * Adds a person to the state.
     */
    override def addPerson(person: Person): PersonOp[Unit] =
      State.modify(_.addPerson(person))

    /**
     * Returns all the people in the state.
     */
    override def getAllPeople(): PersonOp[List[Person]] =
      State.inspect(_.personByName.values.toList)
  }
}
