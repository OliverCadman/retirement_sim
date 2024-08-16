package retcalc.test

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retirement_calculator.SimulatePlanApp


class SimulatePlanAppIT extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  "SimulatePlanApp.strMain" should {
    "return a string displaying the capital accrued at the time of retirement, and at death" in {
      SimulatePlanApp.strMain(
        Array("2016.10:2017.09", "0.5", "0.5", "3000", "2000", "10000")
      ) should === (
        s"""
           |The capital you will have saved before retiring: £17274
           |Your remaining capital once retirement is over: £5737
           |""".stripMargin
      )
    }
    "return an error message in the case that return months are out of bounds" in {
      SimulatePlanApp.strMain(
        Array("1951.09:2017.09", "100", "100", "3000", "2000", "10000")
      ) should === (
        s"Cannot get the return for month \"793\". " +
          s"Accepted range: 0 to 792"
      )
    }
  }
}
