package retcalc.test

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retirement_calculator.{ArgParser, RetCalcParams}
import retirement_calculator.RetCalcError.{InvalidFormatError, InvalidNumberError}
import cats.implicits._

class ArgParseSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  "ArgParser.parseFromUntil" should {
    "return a tuple containing two date IDs, following splitting the arg string via expected delimiter" in {
      ArgParser.parseFromUntil("2020.01:2020.02") should === (Valid("2020.01", "2020.02"))

    }
    "return an InvalidFormat error if the provided arg cannot be split with the specified delimiter" in {
      val badArg = "2020.01?2020.02"
      ArgParser.parseFromUntil(badArg) should === (
        InvalidFormatError("\"fromUntil\"", "\"from:until\"", s"\"$badArg\"").invalidNel
      )
    }
  }
  "ArgParser.parseDouble" should {
    "return a Double from the String type equivalent" in {
      ArgParser.parseDouble("myDouble", "2.5") should === (Valid(2.5))
    }
    "return an InvalidNumber error if the provided arg cannot be converted into a Double type" in {
      val badArg = "2,5"
      ArgParser.parseDouble("myDouble", badArg) should === (
        InvalidNumberError(name = s"\"myDouble\"", value = s"\"$badArg\"").invalidNel
      )
    }
  }
  "ArgParser.parseInt" should {
    "return a Double from the String type equivalent" in {
      ArgParser.parseInt("myInt", "500") should === (Valid(500))
    }
    "return an InvalidNumber error if the provided arg cannot be converted into a Double type" in {
      val badArg = "five-hundred"
      ArgParser.parseInt("myInt", badArg) should === (
        InvalidNumberError(name = s"\"myInt\"", value = s"\"$badArg\"").invalidNel
      )
    }
  }
  "ArgParser.parseParams" should {
    "return an instance of RetCalcParams containing the parsed equivalents of the provided args" in {
      val validFromUntil = "2021.01:2021.12" // Required for array position only
      val validNMonthsSaving = "500" // Required for array position only
      val nYearsInRetirement = "25"
      val income = "1000"
      val expenses = "266"
      val initialCapital = "123456"
      val args = Array(
        validFromUntil, validNMonthsSaving, nYearsInRetirement, income, expenses, initialCapital
      )

      ArgParser.parseParams(args) should === (
        Valid(
          RetCalcParams(
            income = income.toDouble,
            expenses = expenses.toDouble,
            nMonthsInRetirement = (nYearsInRetirement.toDouble * 12).toInt,
            initialCapital = initialCapital.toDouble
          )
        )
      )
    }
    "return an Cats Invalid instance containing a collection of errors" in {
      val validFromUntil = "2021.01:2021.12" // Required for array position only
      val validNMonthsSaving = "500" // Required for array position only
      val invalidNYearsInRetirement = "@5"
      val invalidIncome = "1000,50"
      val validExpenses = "266"
      val invalidInitialCapital = "£$@£$&^%EWff"

      val args = Array(
       validFromUntil, validNMonthsSaving, invalidNYearsInRetirement, invalidIncome, validExpenses, invalidInitialCapital
      )

      ArgParser.parseParams(args) should === (
        Invalid(
          NonEmptyList.of(
            InvalidNumberError("\"nYearsInRetirement\"", s"\"$invalidNYearsInRetirement\""),
            InvalidNumberError("\"monthlyIncome\"", s"\"$invalidIncome\""),
            InvalidNumberError("\"initialCapital\"", s"\"$invalidInitialCapital\"")
          )
        )
      )
    }
  }
}
