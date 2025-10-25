package lib

import lib.quantities.*
import lib.syntax.*

class Libor[T: DateLike](
    val currency: Currency,
    val tenor: Tenor,
    val spotLag: Long,
    val dayCounter: DayCounter,
    val calendar: Calendar[T],
    val resetCurve: YieldCurve[T],
    val bdConvention: BusinessDayConvention
) extends Underlying[T]:

  given DayCounter = dayCounter

  val settlementRule = SettlementRule.simpleRule(spotLag)(using calendar)

  def interestPeriod(from: T) =
    val start = calendar.addBusinessDays(from, spotLag)
    val endDate = calendar.addBusinessPeriod(start, tenor)(using bdConvention)
    start -> endDate

  def forward: Forward[T] =
    t =>
      val (startAt, endAt) = interestPeriod(t)
      val dcf = startAt.yearFractionTo(endAt)
      (1.0 / resetCurve.discount(startAt, endAt) - 1.0) / dcf.toDouble
