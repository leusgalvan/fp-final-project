package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, IsValid, SuccessMsg, readEnv}
import fpfinal.app.Syntax._
import fpfinal.common.Validations._
import fpfinal.model.{Expense, Money, Person}

/**
 * Represents a single command of the application (e.g. Add an expense).
 */
sealed trait Command {
  /**
   * The name of the command, which will be shown to the user.
   */
  val name: String

  /**
   * Executes the command.
   *
   * @return a message to show to the user in case of success (e.g. "Person added successfully!")
   */
  def execute(): AppOp[SuccessMsg]

  /**
   * @return whether or not this is the exit command
   */
  def isExit: Boolean = false
}

object Command {
  implicit val showCommand: Show[Command] = Show.show(_.name)
}

/**
 * A command which exits the application.
 */
object ExitCommand extends Command {
  override val name: String = "Exit app"

  override def isExit: Boolean = true

  override def execute(): AppOp[SuccessMsg] = "Bye :)".pure[AppOp]
}

/**
 * A command which adds an expense to the application state.
 *
 * It asks the user for the payer's name, the amount that was paid,
 * and the list of participants.
 */
object AddExpenseCommand extends Command {
  val name = "Add expense"

  implicit val ME = MonadError[AppOp, String]

  case class AddExpenseData(
      payer: String,
      amount: Double,
      participants: List[String]
  )

  def execute(): AppOp[SuccessMsg] = {
    def readPayer(): AppOp[String] = {
      for {
        env  <- readEnv
        name <- env.console.readLine("Enter payer's name: ").toAppOp
      } yield name
    }

    def readAmount(): AppOp[String] = {
      for {
        env    <- readEnv
        amount <- env.console.readLine("Enter amount: ").toAppOp
      } yield amount
    }

    /**
      * TODO #26: Implement a function that reads participant names from console until the
      * user enters END to finish.
      *
      * Extra points: implement it using ME.tailRecM
      */
    def readParticipants(): AppOp[List[String]] = {
      val msg = "Enter name of participant (or END to finish): "
      ME.tailRecM(List[String]()) { (xs: List[String]) =>
        for {
          env <- readEnv
          participant <- env.console.readLine(msg).toAppOp
        } yield if(participant == "END") Right(xs) else Left(participant :: xs)
      }
    }

    /**
      * TODO #27: Use the helper functions in common.Validations and return a validated
      * instance of AddExpenseData. The validations to perform are:
      * - payer should be a nonempty string
      * - amount should be a valid double
      * - all participants should be nonempty strings
      */
    def validateData(
        payer: String,
        amount: String,
        participants: List[String]
    ): IsValid[AddExpenseData] = {
      (
        nonEmptyString(payer),
        double(amount),
        participants.traverse(nonEmptyString)
      ).mapN { (p, a, ps) =>
        AddExpenseData(p, a, ps)
      }
    }

    def readData(): AppOp[AddExpenseData] = {
      for {
        payer        <- readPayer()
        amount       <- readAmount()
        participants <- readParticipants()
        validData    <- validateData(payer, amount, participants).toAppOp
      } yield validData
    }

    /**
      * TODO #28: Implement a function that finds a person by name.
      *
      * It should be a wrapper of the PersonService analogous function,
      * handling translation between types and converting None results to errors
      * using the provided message (notice the return type is Person, and not
      * Option[Person]).
      *
      * Hint: Use ME.fromOption
      */
    def findPerson(name: String): AppOp[Person] = {
      lazy val msg = s"Person not found: $name"
      for {
        env       <- readEnv
        personOpt <- env.personService.findByName(name).toAppOp
        person    <- ME.fromOption(personOpt, msg)
      } yield person
    }

    for {
      env          <- readEnv
      data         <- readData()
      payer        <- findPerson(data.payer)
      amount       <- Money.dollars(data.amount).toAppOp
      participants <- data.participants.traverse { findPerson }
      expense      <- Expense.create(payer, amount, participants).toAppOp
      _            <- env.expenseService.addExpense(expense).toAppOp
    } yield "Expense created successfully"
  }
}

/**
 * A command which adds a person to the application state.
 *
 * It only asks the user for the name of the person.
 */
case object AddPersonCommand extends Command {
  override val name: String = "Add person"

  case class AddPersonData(name: String)

  override def execute(): AppOp[SuccessMsg] = {
    def validateData(name: String): IsValid[AddPersonData] = {
      nonEmptyString(name).map(AddPersonData.apply)
    }

    def readData(): AppOp[AddPersonData] = {
      for {
        env       <- readEnv
        name      <- env.console.readLine("Enter name: ").toAppOp
        validData <- validateData(name).toAppOp
      } yield validData
    }

    /**
      * TODO #29: Use readData() to read person data from console and use it to add a
      * new person to the state.
      *
      * Upon successful completion, return the message 'Person created successfully'.
      */
    for {
      env    <- readEnv
      data   <- readData()
      person <- Person.create(data.name).toAppOp
      _      <- env.personService.addPerson(person).toAppOp
    } yield "Person created successfully"
  }
}

/**
 * A command which computes the debt given all the people and expenses in the
 * application state and prints a report to console.
 *
 * The DebtByPayer show instance contains the output format.
 */
case object ComputeDebtCommand extends Command {
  override val name: String = "Compute debt"

  override def execute(): AppOp[SuccessMsg] = {
    for {
      env       <- readEnv
      payerDebt <- env.expenseService.computeDebt().toAppOp
      _         <- env.console.printLine(payerDebt.show).toAppOp
    } yield "Debt computed successfully"
  }
}

/**
 * A command which outputs the list of people present in the application state.
 */
case object ListAllPeopleCommand extends Command {
  override val name: String = "List all people"

  override def execute(): AppOp[SuccessMsg] = {
    for {
      env    <- readEnv
      people <- env.personService.getAllPeople().toAppOp
      _      <- env.console.printLine(
                    s"""List of people:
                     |${people.map(p => s"- ${p.show}").mkString("\n")}""".stripMargin
                )
                .toAppOp
    } yield "All people listed!"
  }
}
