package fpfinal.services

import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import fpfinal.FpFinalSpec
import fpfinal.model.Person
import fpfinal.service.PersonService._
import fpfinal.service.{LivePersonService, PersonService}

class PersonServiceSpec extends FpFinalSpec {
  val service: PersonService#Service = new LivePersonService {}.personService

  test("add person adds a person to the state") {
    forAll { (person: Person, initialState: PersonState) =>
      assert(
        service.addPerson(person).runS(initialState).value eqv PersonState(
          initialState.personByName + ((person.name, person))
        )
      )
    }
  }

  test("findByName returns Some when person is found") {
    forAll { (person: Person, initialState: PersonState) =>
      val s = initialState.addPerson(person)
      assert(
        service
          .findByName(person.name)
          .run(s)
          .value eqv (s, Some(person))
      )
    }
  }

  test("findByName returns None when person is not found") {
    forAll { (person: Person, existingPersons: Map[String, Person]) =>
      val initialState = PersonState(existingPersons - person.name)
      assert(
        service
          .findByName(person.name)
          .run(initialState)
          .value eqv (initialState, None)
      )
    }
  }

  test("getAllPeople contains all people") {
    forAll { (initialState: PersonState) =>
      val (resultState, resultPeople) = service
        .getAllPeople()
        .run(initialState)
        .value
      assert(
        (
          resultState,
          resultPeople.toSet
        ) eqv (initialState, initialState.personByName.values.toSet)
      )
    }
  }

  test("PersonState#addPerson adds a person to the map") {
    forAll { (p: Person, personState: PersonState) =>
      assert(
        personState
          .addPerson(p)
          .personByName eqv (personState.personByName + (p.name -> p))
      )
    }
  }
  checkAll("Eq[PersonState]", EqTests[PersonState].eqv)
}
