package lib

import java.time.LocalDate
import lib.quantities.*

class Libor(
    val tenor: Tenor,
    val spotLag: Long,
    val dayCounter: DayCounter,
    val calendar: Calendar,
    val resetCurve: Curve,
    val bdConvention: BusinessDayConvention
) extends Underlying:

  def interestPeriod(from: LocalDate) =
    val start   = calendar.addBusinessDays(from, spotLag)
    val endDate = calendar.addBusinessPeriod(start, tenor)(using bdConvention)
    start -> endDate

  def forward(date: LocalDate)(using Market): Either[Error, Double] = ???
