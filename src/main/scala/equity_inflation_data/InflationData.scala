package equity_inflation_data

case class InflationData(monthId: String, cpi: Double)

object InflationData {
  def fromResource(path: String): Seq[InflationData] = {
    scala.io.Source.fromResource(path).getLines().drop(1).map {
      line => {
        val fields = line.split("\t")
        val monthId = fields(0)
        val cpi = fields(1).toDouble

        InflationData(monthId, cpi)
      }
    }.toSeq
  }
}
