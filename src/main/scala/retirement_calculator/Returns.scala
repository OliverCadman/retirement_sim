package retirement_calculator

import equity_inflation_data.{EquityData, InflationData}

import scala.annotation.tailrec

sealed trait Returns
case class FixedReturns(annualInterest: Double) extends Returns

case class VariableReturn(monthId: String, monthlyInterest: Double)
case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {
  def fromUntil(from: String, until: String): VariableReturns =
    VariableReturns(
      returns
        .dropWhile(_.monthId != from)
        .takeWhile(_.monthId != until)
    )
}
case class OffsetReturns(orig: Returns, month: Int) extends Returns


object Returns {
  @tailrec
  def monthlyRate(returns: Returns, month: Int): Either[RetCalcError, Double] = returns match {
    case FixedReturns(returns) => Right(returns / 12)
    case VariableReturns(returns) => {
      if (month > returns.length) Left(RetCalcError.MonthOutOfBoundsError(month, returns.length))
      else Right(returns(month % returns.length).monthlyInterest)
    }
    case OffsetReturns(returns, offset) => Returns.monthlyRate(returns, month + offset)
  }

  def fromEquityAndInflationData(equityData: Seq[EquityData],
                                 inflationData: Seq[InflationData]
                                ): VariableReturns = {

    VariableReturns(
      equityData.zip(inflationData).sliding(2).map {
        case prev +: curr +: Nil => {
          val prevEquity = prev._1
          val prevInflation = prev._2
          val currEquity = curr._1
          val currInflation = curr._2

          val realEquity = ((currEquity.sp500 + currEquity.dividend / 12)) / prevEquity.sp500 - currInflation.cpi / prevInflation.cpi
          VariableReturn(currEquity.month, realEquity)
        }
      }.toVector
    )
  }
}
