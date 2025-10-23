package lib

import lib.quantities.*
import lib.syntax.*

class Libor[T: DateLike](
    val name: String,
    val currency: Currency,
    val tenor: Tenor,
    val spotLag: Long,
    val dayCounter: DayCounter,
    val calendar: Calendar[T],
    val resetWith: Curve,
    val bdConvention: BusinessDayConvention
) extends Underlying[T]:

  given DayCounter = dayCounter

  val settlementRule = SettlementRule.simpleRule(spotLag)(using calendar)

  def interestPeriod(from: T) =
    val start = calendar.addBusinessDays(from, spotLag)
    val endDate = calendar.addBusinessPeriod(start, tenor)(using bdConvention)
    start -> endDate

  def forward(using Market[T]): Either[Error, Forward[T]] =
    summon[Market[T]].yieldCurve(resetWith).map: yieldCurve =>
      t =>
        val (startAt, endAt) = interestPeriod(t)
        val dcf = startAt.yearFractionTo(endAt)
        (1.0 / yieldCurve.discount(startAt, endAt) - 1.0) / dcf.toDouble
