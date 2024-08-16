package retcalc.test

import equity_inflation_data.{EquityData, InflationData}
import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.EitherValues
import retirement_calculator.{FixedReturns, OffsetReturns, RetCalcError, Returns, VariableReturn, VariableReturns}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReturnsSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals with EitherValues {

  implicit val doubleEquality: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.0001)

  "Returns.monthlyRate" should {
    "Return a fixed rate for a fixed return" in {
        Returns.monthlyRate(FixedReturns(0.04), 0).value should === (0.04 / 12)
        Returns.monthlyRate(FixedReturns(0.04), 1).value should === (0.04 / 12)
    }

    val variableReturns = VariableReturns(
      Vector(
        VariableReturn("2000.01", 0.1),
        VariableReturn("2000.02", 0.2)
      )
    )

    "Return the nth rate for a variable return" in {
      Returns.monthlyRate(variableReturns, 0).value should === (0.1)
      Returns.monthlyRate(variableReturns, 1).value should === (0.2)
    }

    "Return error if month out of bounds" in {
      Returns.monthlyRate(variableReturns, 3).left.value should === (RetCalcError.MonthOutOfBoundsError(3, 2))
      Returns.monthlyRate(variableReturns, 6).left.value should === (RetCalcError.MonthOutOfBoundsError(6, 2))
    }

    "Return the nth + offset return" in {
      val offsetReturns = OffsetReturns(variableReturns, 1)
      Returns.monthlyRate(offsetReturns, 0).value should === (0.2)
    }

    "Return a month out of bounds error if the offset falls outside of size of returns" in {
      val offsetReturns = OffsetReturns(variableReturns, 2)
      Returns.monthlyRate(offsetReturns, 1).left.value should === (RetCalcError.MonthOutOfBoundsError(3, 2))
    }

  }

  "Returns.fromEquityAndInflationData" should {
    /*
       realReturns(n) = (price(n) + dividends(n) / 12) / price(n-1) - inflation(n) / inflation(n - 1)
      */

    val equityData = Seq(
      EquityData("2000.01", 125.5, 12.0),
      EquityData("2000.02", 119.73, 12.3),
      EquityData("2000.03", 121.44, 11.6)
    )

    val inflationData = Seq(
      InflationData("2000.01", 241.428),
      InflationData("2000.02", 241.729),
      InflationData("2000.03", 241.353),
    )

    "return the correct Variable Returns from Equity and Inflation case classes" in {
      Returns.fromEquityAndInflationData(equityData, inflationData) should === (
        VariableReturns(
          Vector(
            VariableReturn("2000.02", ((119.73 + 12.3 / 12) / 125.5) - (241.729 / 241.428)),
            VariableReturn("2000.03", ((121.44 + 11.6 / 12) / 119.73) - 241.353 / 241.729)
          )
        )
      )
    }
  }

  "VariableReturns.fromUntil" should {
    "return the correct subset of Variable Returns" in {
      val returns = VariableReturns(
        Vector.tabulate(12)(x => {
          val n = (x + 1).toDouble
          VariableReturn(f"2000.$n%02.0f", n)
        })
      )

      returns.fromUntil("2000.01", "2000.05") should === (
        VariableReturns(
          Vector(
            VariableReturn("2000.01", 1.0),
            VariableReturn("2000.02", 2.0),
            VariableReturn("2000.03", 3.0),
            VariableReturn("2000.04", 4.0)
          )
        )
      )
    }
  }
}
