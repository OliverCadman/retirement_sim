package retirement_calculator

import cats.data.ValidatedNel

abstract class RetCalcError(val message: String)

object RetCalcError {

  type RetCalcResult[A] = ValidatedNel[RetCalcError, A]

  case class NegativeEquityError(income: Double, expenses: Double) extends RetCalcError(
    message = s"Expenses are greater than income. Expenses: $expenses, Income: $income"
  )

  case class MonthOutOfBoundsError(month: Int, max: Int) extends RetCalcError(
    message = s"Cannot get the return for month \"$month\". " +
      s"Accepted range: 0 to $max"
  )

  case class InvalidFormatError(name: String, expected: String, actual: String) extends RetCalcError(
    message = s"Invalid format for argument $name. Expected: $expected. Actual: $actual."
  )

  case class InvalidNumberError(name: String, value: String) extends RetCalcError(
    message = s"Invalid number for argument $name: $value."
  )
}