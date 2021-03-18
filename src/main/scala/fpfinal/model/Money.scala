package fpfinal.model

import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._

class Money private (_cents: Int) {
  def cents: Int = _cents
  def dollars: Int = _cents / 100
}

object Money {
  def dollars(amount: Double): IsValid[Money] =
    positive(amount).map { dls =>
      val cents = (dls * 100).toInt
      new Money(cents)
    }
}
