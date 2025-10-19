package lib

import lib.quantities.*

import java.time.LocalDate

class Libor(
    val name: String,
    val currency: Currency,
    val tenor: Tenor,
    val spotLag: Long,
    val dayCounter: DayCounter,
    val calendar: Calendar,
    val resetCurve: Curve,
    val bdConvention: BusinessDayConvention
) extends Underlying:

  val settlementRule = SettlementRule.simpleRule(spotLag)(using calendar)

  def interestPeriod(from: LocalDate) =
    val start = calendar.addBusinessDays(from, spotLag)
    val endDate = calendar.addBusinessPeriod(start, tenor)(using bdConvention)
    start -> endDate

  def forward(using Market): Either[Error, Forward] =
    summon[Market].yieldCurve(resetCurve).map: yieldCurve =>
      t =>
        val (start, end) = interestPeriod(t)
        val dcf = dayCounter.yearFraction(start, end)
        (yieldCurve.discount(start, end) - 1.0) / dcf.toDouble
