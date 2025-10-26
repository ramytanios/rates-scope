package lib

import lib.BusinessDayConvention.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class SwapRateSuite extends munit.FunSuite:

  test("forward should match expected value"):

    val t = d"2025-10-21"

    val resetCurve = YieldCurve.continuousCompounding(t, 0.02, DayCounter.Act365)
    val discountCurve = resetCurve

    val floatingRate = new Libor(
      Currency.EUR,
      Tenor.`3M`,
      2,
      DayCounter.Act360,
      Calendar.all,
      resetCurve,
      ModifiedFollowing
    )

    val rate = new SwapRate(
      Tenor.years(5),
      2,
      0,
      Tenor.`1Y`,
      floatingRate,
      DayCounter.Act360,
      Calendar.all,
      ModifiedFollowing,
      StubConvention.Short,
      Direction.Backward,
      discountCurve
    )

    val fixingAt = Calendar.all.addBusinessPeriod(t, Tenor.`1Y`)(using ModifiedFollowing)
    assertEqualsDouble(rate.forward(fixingAt), 0.019924721293744743, 1e-12)
