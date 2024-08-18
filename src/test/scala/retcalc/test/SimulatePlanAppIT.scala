package retcalc.test

import cats.data.Validated.{Invalid, Valid}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retirement_calculator.SimulatePlanApp


class SimulatePlanAppIT extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  "SimulatePlanApp.strMain" should {
    "return a string displaying the capital accrued at the time of retirement, and at death" in {
      val result = SimulatePlanApp.strMain(
        Array("2016.10:2017.09", "0.5", "0.5", "3000", "2000", "10000")
      )

      val expected = s"""
                        |The capital you will have saved before retiring: £17274
                        |Your remaining capital once retirement is over: £5737
                        |""".stripMargin

      result should === (Valid(expected))
    }
    // TODO: FIX THIS
//    "return an error message in the case that return months are out of bounds" in {
//      val result = SimulatePlanApp.strMain(
//        Array("1951.09:2017.09", "100", "100", "3000", "2000", "10000")
//      )
//
//      val expected =   s"Cannot get the return for month \"793\". " +
//        s"Accepted range: 0 to 792"
//
//      result.leftMap(x => x should === (expected))
//    }

    "return an Validated 'Invalid' instance describing the usage of the program" in {
      val result = SimulatePlanApp.strMain(
        Array("1951.09,2017.09", "30", "40", "3000", "2000")
      )

      val expected =
        """
          |Usage:
          | - Arg 1: from:until
          | - Arg 2: Number of years saving
          | - Arg 3: Number of years in retirement
          | - Arg 4: Monthly income
          | - Arg 5: Monthly expenses
          | - Arg 6: Initial Capital
          |
          |Example: 1951.09:2017.09, 30, 40, 3000, 2000, 10000
          |""".stripMargin

      result should === (Invalid(expected))
    }

    "return a Validated 'Invalid' instance describing the incorrect arguments" in {
      val result = SimulatePlanApp.strMain(
        Array("1951.09,2017.09", "30", "40", "3,000", "two thousand", "1,0,0,0,0")
      )

      val expected =
        """Invalid format for argument "fromUntil". Expected: "from:until". Actual: "1951.09,2017.09".
          |Invalid number for argument "monthlyIncome": "3,000".
          |Invalid number for argument "monthlyExpenses": "two thousand".
          |Invalid number for argument "initialCapital": "1,0,0,0,0".""".stripMargin

      result should === (Invalid(expected))
    }
  }
}
