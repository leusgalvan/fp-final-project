package fpfinal.model

import fpfinal.app.Configuration.IsValid

class Money private (_cents: Int) {
  def cents: Int = _cents
  def dollars: Int = _cents / 100
}

object Money {
  def dollars(amount: Double): IsValid[Money] = ???
}
