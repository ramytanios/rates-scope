package lib

import lib.DayCounter.Act360
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class CapletSuite extends munit.FunSuite:

  test("caplet price"):

    val t = d"2025-10-12"

    val resetCurve = YieldCurve.continousCompounding(t, 0.02, DayCounter.Act365)
    val discountCurve = resetCurve

    val libor = new Libor(
      Currency.USD,
      Tenor.`3M`,
      2,
      Act360,
      Calendar(),
      resetCurve,
      BusinessDayConvention.ModifiedFollowing
    )

    val fixingAt = DateLike[LocalDate].plusPeriod(t, Tenor.`1Y`)
    val (startAt, endAt) = libor.interestPeriod(fixingAt)

    val volSurface = VolatilitySurface.fromMoneynessSkew(
      libor.forward,
      Seq(-0.02, -0.01, -0.005, -0.0025, 0.0, 0.0025, 0.005, 0.01, 0.02),
      Seq(100.0, 80.0, 72.0, 70.0, 69.0, 71.0, 74.0, 90.0, 93.0).map(_ / 10000)
    )

    val caplet = new Caplet(
      libor,
      fixingAt,
      startAt,
      endAt,
      endAt,
      Currency.USD,
      libor.forward(fixingAt) / 2.0,
      discountCurve,
      OptionType.Call
    )

    caplet.price(t, volSurface).foreach: price =>
      assertEqualsDouble(price, 0.0025670413971979954, 1e-10)
