package retirement_calculator

import equity_inflation_data.{EquityData, InflationData}

object SimulatePlanApp extends App {

  /**
   *
   * @param args
   *  from:until - A slice of time to select from the returns TSV data
   *  nMonthsSaving: The number of months a person plans to save
   *  income: A person's income
   *  expenses: A person's expenses
   *  initialCapital: How much the person already has saved.
   *
   */
  def strMain(args: Array[String]): String = {
    val from +: until +: Nil = args(0).split(":").toList
    val nYearsSaving = args(1)
    val nYearsInRetirement = args(2)
    val income = args(3)
    val expenses = args(4)
    val initialCapital = args(5)

    val allReturns = Returns.fromEquityAndInflationData(
      EquityData.fromResource("sp500.tsv"),
      InflationData.fromResource("cpi.tsv")
    )

    RetCalc.simulatePlan(
      returns = allReturns.fromUntil(from, until),
      nMonthsSaving = (nYearsSaving.toDouble * 12).toInt,
      params = RetCalcParams(
        income = income.toDouble,
        expenses = expenses.toDouble,
        nMonthsInRetirement = (nYearsInRetirement.toDouble * 12).toInt,
        initialCapital = initialCapital.toDouble
      )
    ) match {
      case Right((capitalAfterRetirement, capitalAfterDeath)) =>
        s"""
           |The capital you will have saved before retiring: £${capitalAfterRetirement.round}
           |Your remaining capital once retirement is over: £${capitalAfterDeath.round}
           |""".stripMargin

      case Left(err) => err.message
    }

  }
}
