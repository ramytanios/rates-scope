package lib

import lib.quantities.*

import java.time.LocalDate

class Libor(
    val tenor: Tenor,
    val spotLag: Long,
    val dayCounter: DayCounter,
    val calendar: Calendar,
    val resetCurve: Curve,
    val bdConvention: BusinessDayConvention
) extends Underlying:

  def interestPeriod(from: LocalDate) =
    val start = calendar.addBusinessDays(from, spotLag)
    val endDate = calendar.addBusinessPeriod(start, tenor)(using bdConvention)
    start -> endDate

  def forward(t: LocalDate)(using market: Market): Either[Error, Double] =
    market.yieldCurve(resetCurve).map: yieldCurve =>
      val (_start, _end) = interestPeriod(t)
      val dcf = dayCounter.yearFraction(_start, _end)
      (yieldCurve.discount(_start, _end) - 1.0) / dcf.toDouble
