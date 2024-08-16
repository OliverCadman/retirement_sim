package retirement_calculator

import scala.annotation.tailrec

object RetCalc {

  def futureCapital(
                   returns: Returns,
                   income: Double,
                   expenses: Double,
                   nMonthsSaving: Int,
                   initialCapital: Double
                   ): Either[RetCalcError, Double] = {
    val monthlySavings = income - expenses

    (0 until nMonthsSaving).foldLeft[Either[RetCalcError, Double]](Right(initialCapital)) {
        case (accumulated, month) =>
          for {
            acc <- accumulated
            monthlyRate <- Returns.monthlyRate(returns, month)
          } yield acc * (1 + monthlyRate) + monthlySavings
      }
  }

  def simulatePlan(returns: Returns,
                   nMonthsSaving: Int,
                   params: RetCalcParams
                  ): Either[RetCalcError, (Double, Double)] = {

    import params._

    for {
      capitalAfterRetirement <- futureCapital(
        returns,
        income,
        expenses,
        nMonthsSaving,
        initialCapital
      )
      capitalAfterDeath <- futureCapital(
        OffsetReturns(returns, nMonthsSaving),
        0,
        expenses,
        nMonthsInRetirement,
        capitalAfterRetirement
      )
    } yield (capitalAfterRetirement, capitalAfterDeath)

  }

  def nbMonthsSaving(
                    returns: Returns,
                    params: RetCalcParams
                    ): Either[RetCalcError, Int] = {

    import params.{income, expenses}

    @tailrec
    def calcSavings(months: Int): Either[RetCalcError, Int] = {

      val retirementPlan = simulatePlan(
        returns,
        months,
        params
      )

      retirementPlan match {
        case Right((_, capitalAfterDeath)) => {
          if (capitalAfterDeath > 0.0) Right(months)
          else calcSavings(months + 1)
        }
        case Left(err) => Left(err)
      }

    }

    if (income < expenses) Left(RetCalcError.NegativeEquityError(income, expenses))
    else calcSavings(0)
  }

}
