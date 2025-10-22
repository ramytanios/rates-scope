package lib

import lib.BusinessDayConvention.*
import lib.Schedule.Direction
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class SwapRateSuite() extends munit.FunSuite:

  val ref = d"2025-10-21"

  val floatingRate = new Libor(
    "Euribor_3M",
    Currency.EUR,
    Tenor.`3M`,
    0,
    DayCounter.Act360,
    Calendar(),
    Curve(Currency.EUR, "reset_curve"),
    ModifiedFollowing
  )

  val rate = new SwapRate(
    "swap_rate",
    Tenor.`2Y`,
    0,
    0,
    Tenor.`3M`,
    floatingRate,
    DayCounter.Act360,
    Calendar(),
    ModifiedFollowing,
    lib.Schedule.StubConvention.Short,
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
    Map.empty
  )

  rate.forward.foreach: forward =>
    val t = Calendar().addBusinessPeriod(ref, Tenor.`1Y`)(using ModifiedFollowing)
    assertEqualsDouble(forward(t), 0.019775494583490195, 1e-12)
