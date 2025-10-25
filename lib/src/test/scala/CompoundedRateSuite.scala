package lib

import lib.BusinessDayConvention.*
import lib.DayCounter.Act360
import lib.literals.*
import lib.quantities.Rate
import lib.quantities.Tenor

class CompoundedRateSuite extends munit.FunSuite with lib.EitherSyntax:

  test("forward should match expected value"):

    val tol = 1e-12

    val t = d"2025-09-01"

    val calendar = Calendar.weekends

    val resetCurve = YieldCurve.continuousCompounding(t, 0.05, DayCounter.Act365)

    val dailyRate = new Libor(Currency.USD, Tenor.`1D`, 0, Act360, calendar, resetCurve, Following)

    val stub = Schedule.StubConvention.Long

    val direction = Schedule.Direction.Forward

    val fixings = Map(
      d"2025-08-26" -> 0.05,
      d"2025-08-27" -> 0.05,
      d"2025-08-28" -> 0.05,
      d"2025-08-29" -> 0.05,
      d"2025-09-01" -> 0.05
    )

    CompoundedRate(d"2025-09-02", d"2025-09-05", dailyRate, stub, direction).forward(t, fixings)
      .failOrAssert: fwd =>
        assertEqualsDouble(fwd, 0.0493252031146518, tol, s"t < T_0")

    CompoundedRate(d"2025-08-29", d"2025-09-05", dailyRate, stub, direction).forward(t, fixings)
      .failOrAssert: fwd =>
        assertEqualsDouble(fwd, 0.0497255228446493, tol, s"T_k <= t < T_{k+1}")

    CompoundedRate(d"2025-08-26", d"2025-09-02", dailyRate, stub, direction).forward(t, fixings)
      .failOrAssert: fwd =>
        assertEqualsDouble(fwd, 0.050017860174422, tol, s"t = T_n")
