package retirement_calculator

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId}
import retirement_calculator.RetCalcError.RetCalcResult

import scala.annotation.tailrec

object RetCalc {

  def futureCapital(
                   returns: Returns,
                   income: Double,
                   expenses: Double,
                   nMonthsSaving: Int,
                   initialCapital: Double
                   ): RetCalcResult[Double] = {
    val monthlySavings = income - expenses

    (0 until nMonthsSaving).foldLeft[RetCalcResult[Double]](Valid(initialCapital)) {
        case (accumulated, month) =>
          val vReturns = Validated
            .fromEither(Returns.monthlyRate(returns, month))
            .asInstanceOf[RetCalcResult[Double]]

          (accumulated, vReturns).mapN {
            case (acc, monthlyRate) => (acc * (1 + monthlyRate) + monthlySavings)
          }
        }
  }

  def simulatePlan(returns: Returns,
                   nMonthsSaving: Int,
                   params: RetCalcParams
                  ): RetCalcResult[(Double, Double)] = {

    import params._

    val capitalAfterRetirement = futureCapital(
      returns,
      income,
      expenses,
      nMonthsSaving,
      initialCapital
    )

    capitalAfterRetirement.andThen {
      capitalAtRetirement => {
        val capitalAfterDeath = futureCapital(
          OffsetReturns(returns, nMonthsSaving),
          0,
          expenses,
          nMonthsInRetirement,
          capitalAtRetirement
        )

          capitalAfterDeath.map {
            capitalAtDeath => (capitalAtRetirement, capitalAtDeath)
          }

      }
    }
  }

  def nbMonthsSaving(
                    returns: Returns,
                    params: RetCalcParams
                    ): RetCalcResult[Int] = {

    import params.{income, expenses}

    @tailrec
    def calcSavings(months: Int): RetCalcResult[Int] = {

      val retirementPlan = simulatePlan(
        returns,
        months,
        params
      )

      retirementPlan match {
        case Valid((_, capitalAfterDeath)) => {
          if (capitalAfterDeath > 0.0) Valid(months)
          else calcSavings(months + 1)
        }
        case Invalid(err) => Invalid(err)
      }

    }

    if (income < expenses) RetCalcError.NegativeEquityError(income, expenses).invalidNel
    else calcSavings(0)
  }

}
