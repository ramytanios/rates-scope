package lib

import lib.BusinessDayConvention.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class CompoundedSwapRateSuite extends munit.FunSuite:
  
  test("forward should match expected value"):

    val ref = d"2025-10-21"

    val floatingRate = new Libor(
      "floating_rate",
      Currency.EUR,
      Tenor.`3M`,
      2,
      DayCounter.Act360,
      Calendar(),
      Curve(Currency.EUR, "reset_curve"),
      ModifiedFollowing
    )

    val rate = new CompoundedSwapRate(
      "swap_rate",
      Tenor.years(5),
      2,
      0,
      Tenor.`1Y`,
      floatingRate,
      Tenor.`1Y`,
      DayCounter.Act360,
      Calendar(),
      ModifiedFollowing,
      StubConvention.Short,
      Direction.Backward,
      Curve(Currency.EUR, "discounting_curve")
    )

    val yieldCurve = YieldCurve.continousCompounding(ref, 0.02, DayCounter.Act365)

    given Market[LocalDate] = Market(
      ref,
      Map(
        Curve(Currency.EUR, "reset_curve") -> yieldCurve,
        Curve(Currency.EUR, "discounting_curve") -> yieldCurve
      ),
      Map.empty,
      Map.empty,
      Map.empty
    )

    rate.forward.foreach: forward =>
      val t = Calendar().addBusinessPeriod(ref, Tenor.`1Y`)(using ModifiedFollowing)
      assertEqualsDouble(forward(t), 0.019924721293744743, 1e-12)
