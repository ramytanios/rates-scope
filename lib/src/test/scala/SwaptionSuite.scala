package lib

import lib.DayCounter.Act360
import lib.dtos.*
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class SwaptionSuite extends munit.FunSuite with lib.EitherSyntax:

  test("swaption price"):

    val t = d"2025-11-01"

    val resetCurve = YieldCurve.continuousCompounding(t, 0.02, DayCounter.Act365)
    val discountCurve = resetCurve

    val libor = new Libor(
      Currency.USD,
      Tenor.`3M`,
      2,
      Act360,
      Calendar.all,
      resetCurve,
      BusinessDayConvention.ModifiedFollowing
    )

    val swapRate = new SwapRate(
      Tenor.`2Y`,
      0,
      0,
      Tenor.`3M`,
      libor,
      Act360,
      Calendar.all,
      BusinessDayConvention.ModifiedFollowing,
      StubConvention.Short,
      Direction.Backward,
      discountCurve
    )

    val fixingAt = DateLike[LocalDate].plusPeriod(t, Tenor.`1Y`)
    val (startAt, endAt) = libor.interestPeriod(fixingAt)

    val volSurface = VolatilitySurface.fromMoneynessSkew(
      libor.forward,
      Seq(-0.02, -0.01, -0.005, -0.0025, 0.0, 0.0025, 0.005, 0.01, 0.02),
      Seq(100.0, 80.0, 72.0, 70.0, 69.0, 71.0, 74.0, 90.0, 93.0).map(_ / 10000)
    )

    val caplet = new Swaption(
      swapRate,
      fixingAt,
      libor.forward(fixingAt) / 2.0,
      OptionType.Call,
      Annuity.Physical,
      discountCurve,
      lib.Detachment.default
    )

    caplet.price(t, volSurface, Map.empty).failOrAssert: price =>
      assertEqualsDouble(price, 0.02004611618815355, 1e-10)
