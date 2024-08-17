package retirement_calculator

import RetCalcError._
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated}
import cats.implicits._

object ArgParser {

  def parseParams(params: Array[String]): RetCalcResult[RetCalcParams] = {
    (
      parseDouble("nYearsInRetirement", params(2)),
      parseInt("monthlyIncome", params(3)),
      parseInt("monthlyExpenses", params(4)),
      parseDouble("initialCapital", params(5))
    ).mapN {
      case (nYearsInRetirement, monthlyIncome, monthlyExpenses, initialCapital) => RetCalcParams(
        nMonthsInRetirement = (nYearsInRetirement * 12).toInt,
        income = monthlyIncome,
        expenses = monthlyExpenses,
        initialCapital = initialCapital
      )
    }
  }
  def parseFromUntil(fromUntil: String): RetCalcResult[(String, String)] = {
    val arr = fromUntil.split(":")
    if (arr.length != 2) {
      InvalidFormatError(name = "\"fromUntil\"", expected = "\"from:until\"", actual=s"\"$fromUntil\"").invalidNel
    } else (arr(0), arr(1)).validNel
  }
  def parseInt(name: String, value: String): RetCalcResult[Int] = {
    Validated
      .catchOnly[NumberFormatException] (value.toInt)
      .leftMap(_ => NonEmptyList.of(InvalidNumberError(name = s"\"$name\"", value = s"\"$value\"")))

  }
  def parseDouble(name: String, value: String): RetCalcResult[Double] = {
    Validated
      .catchOnly[NumberFormatException] (value.toDouble)
      .leftMap(_ => NonEmptyList.of(InvalidNumberError(name = s"\"$name\"", value = s"\"$value\"")))
  }
}
