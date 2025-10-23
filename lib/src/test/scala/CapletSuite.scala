package lib

import java.time.LocalDate
import lib.quantities.Tenor
import lib.DayCounter.Act360

class CapletSuite extends munit.FunSuite:

  test("caplet price"):

    val resetWith = Curve(Currency.USD, "reset_curve")
    val discountWith = Curve(Currency.USD, "discount_curve")

    val libor = new Libor(
      "libor",
      Currency.USD,
      Tenor.`3M`,
      2,
      Act360,
      Calendar(),
      resetWith,
      BusinessDayConvention.ModifiedFollowing
    )

    // needs market, but market needs vol :/.
    // pass directly yield curve to forward computations ?
    // or `YieldCurves` provider like

    val volSurface = VolatilitySurface.fromMoneynessSkew(
      libor.forward,
      Seq(-0.02, -0.01, -0.005, -0.0025, 0.0, 0.0025, 0.005, 0.01, 0.02),
      Seq(100.0, 80.0, 72.0, 70.0, 69.0, 71.0, 74.0, 90.0, 93.0).map(_ / 10000)
    )
