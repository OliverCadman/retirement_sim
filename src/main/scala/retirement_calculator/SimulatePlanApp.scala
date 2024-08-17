package retirement_calculator

import cats.data.Validated
import equity_inflation_data.{EquityData, InflationData}
import retirement_calculator.RetCalcError.RetCalcResult
import cats.implicits._

object SimulatePlanApp extends App {

  import ArgParser._

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
  def strMain(args: Array[String]): Validated[String, String] = {

    if (args.length != 6) {
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
        |""".stripMargin.invalid
    } else {

      val allReturns = Returns.fromEquityAndInflationData(
        EquityData.fromResource("sp500.tsv"),
        InflationData.fromResource("cpi.tsv")
      )

      val vFromUntil = parseFromUntil(args(0))
      val vNYearsSaving = parseDouble("nYearsSaving", args(1))
      val vParams = parseParams(args)
      println(vFromUntil)
      println(vNYearsSaving)
      println(vParams)

      (vFromUntil, vNYearsSaving, vParams)
        .tupled
        .andThen {
          case ((from, until), nbYearsSaving, params) =>
            strSimulatePlan(allReturns.fromUntil(from, until), nbYearsSaving, params)
        }
        .leftMap(nel => nel.map(_.message).toList.mkString("\n"))
    }

  }

  def strSimulatePlan(returns: Returns, nYearsSaving: Double, params: RetCalcParams): RetCalcResult[String] = {
    RetCalc.simulatePlan(
      returns = returns,
      nMonthsSaving = (nYearsSaving * 12).toInt,
      params = params
    ).map {
      case (capitalAfterRetirement, capitalAfterDeath) =>
        s"""
           |The capital you will have saved before retiring: £${capitalAfterRetirement.round}
           |Your remaining capital once retirement is over: £${capitalAfterDeath.round}
           |""".stripMargin
    }.toValidatedNel
  }
}
