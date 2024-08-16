package retirement_calculator

abstract class RetCalcError(val message: String)

object RetCalcError {
  case class NegativeEquityError(income: Double, expenses: Double) extends RetCalcError(
    message = s"Expenses are greater than income. Expenses: $expenses, Income: $income"
  )

  case class MonthOutOfBoundsError(month: Int, max: Int) extends RetCalcError(
    message = s"Cannot get the return for month \"$month\". " +
      s"Accepted range: 0 to $max"
  )
}