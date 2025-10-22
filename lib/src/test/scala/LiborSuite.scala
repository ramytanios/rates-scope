package lib

import lib.BusinessDayConvention.*
import lib.literals.*
import lib.quantities.Rate
import lib.quantities.Tenor

import java.time.LocalDate

class LiborSuite() extends munit.FunSuite:

  val ref = d"2025-10-12"

  val libor = new Libor(
    "SOFR",
    Currency.USD,
    Tenor.`1D`,
    0,
    DayCounter.Act360,
    Calendar(),
    Curve(Currency.USD, "SOFR"),
    ModifiedFollowing
  )

  val yieldCurve = YieldCurve.continousCompounding(ref, 0.02, DayCounter.Act365)

  given Market[LocalDate] = Market.fromSingleCurve(ref, Currency.USD, "SOFR", yieldCurve)

  libor.forward.foreach: forward =>
    val t = Calendar().addBusinessPeriod(ref, Tenor.`1Y`)(using ModifiedFollowing)
    assertEqualsDouble(forward(t), 0.019726567846252152, 1e-12)
