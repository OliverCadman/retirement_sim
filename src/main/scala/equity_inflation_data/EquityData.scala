package equity_inflation_data

case class EquityData(month: String, sp500: Double, dividend: Double)

object EquityData {
  def fromResource(path: String): Seq[EquityData] = {
    scala.io.Source.fromResource(path).getLines().drop(1).map {
      line => {
        val data = line.split("\t")
        val month = data(0)
        val sp500 = data(1).toDouble
        val dividend = data(2).toDouble

        EquityData(month, sp500, dividend)
      }
    }.toSeq
  }
}
