package retcalc.test

import cats.data.Validated.Valid
import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retirement_calculator.RetCalcError.RetCalcResult
import retirement_calculator.{FixedReturns, RetCalc, RetCalcError, RetCalcParams, VariableReturn, VariableReturns}

class RCSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals
  with EitherValues {

  implicit val doubleEquality: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.1)

  val baseRetCalcParams: RetCalcParams = RetCalcParams(
    income = 3000,
    expenses = 2000,
    nMonthsInRetirement = 40 * 12,
    initialCapital = 10000
  )

  "Retcalc.futureCapital" should {
    "Calculate the amount of capital I will accrue after n months saving" in {

      val expected = 508334.41
      val result = RetCalc.futureCapital(
        returns = FixedReturns(0.04),
        income = 3000,
        expenses = 2000,
        nMonthsSaving = 24 * 12,
        initialCapital = 10000
      )

      result.map(_ should === (expected))

    }

    "Calculate the amount of capital I will have after n months in retirement" in {
      val expected = 147183.80

      val result = RetCalc.futureCapital(
        returns = FixedReturns(0.04),
        income = 0,
        expenses = 2000,
        nMonthsSaving = 40 * 12,
        initialCapital = 508334.41
      )

      result.map(_ should === (expected))
    }
  }

  "RetCalc.simulatePlan" should {
    "calculate the correct amount of capital before and after retirement" in {
      val expectedCapitalAfterRetirement = 508334.41
      val expectedCapitalAfterDeath = 147183.80

      RetCalc.simulatePlan(
        returns = FixedReturns(0.04),
        nMonthsSaving = 24 * 12,
        params = baseRetCalcParams
      ).map(first => {
        first._1 should === (expectedCapitalAfterRetirement)
        first._2 should === (expectedCapitalAfterDeath)
      })

    }

  "Calculate the amount of capital I will accrue and be left with in drawdown" in {
    val nMonths = 25 * 12

    val returns = VariableReturns(
      Vector.tabulate(nMonths + baseRetCalcParams.nMonthsInRetirement)(x => {
        if (x < nMonths) VariableReturn(x.toString, 0.04 / 12)
        else VariableReturn(x.toString, 0.03 / 12)
      })
    )

    RetCalc.simulatePlan(returns, nMonths, params = baseRetCalcParams).foreach {
      result =>
        val capitalAfterRetirement = result._1
        val capitalAfterDeath = result._2

        capitalAfterRetirement should === (541267.20)
        capitalAfterDeath should === (-57737.72)
    }
  }




  "RetCalc.nbMonthsSaving" should {
    "Calculate the numbers of months it will take to accrue n capital" in {
      /**
       * params:
       *
       * returns
       * nMonthsInRetirement
       * income
       * expenses
       * initialCapital
       */

      val nMonthsSaving = RetCalc.nbMonthsSaving(
        returns = FixedReturns(0.04),
        params = baseRetCalcParams
      )


      nMonthsSaving.map(_ should === (23 * 12 + 1))
    }

    "Should not choke up if the number of months ends up being very high" in {
      val retCalcParams = baseRetCalcParams.copy(
        expenses = 2999,
        initialCapital = 0
      )
      val nMonths = RetCalc.nbMonthsSaving(
        returns = FixedReturns(0.01),
        params = retCalcParams
      )

      nMonths.map(_ should === (8280))
    }

    "Should return an error if the amount of expenses is greater than the amount of income" in {
      val retCalcParams = baseRetCalcParams.copy(
        expenses = 3001
      )

      val nMonths = RetCalc.nbMonthsSaving(
        returns = FixedReturns(0.04),
        params = retCalcParams
      )

      nMonths.leftMap(x => x.head should === (RetCalcError.NegativeEquityError(3000, 3001)))
    }
  }
  }
}
