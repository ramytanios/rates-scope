package lib

import java.time.LocalDate
import lib.quantities.*
import lib.quantities.Tenor.*

trait Calendar:

  def isBusinessDay(date: LocalDate): Boolean

  def addBusinessDays(date: LocalDate, days: Long): LocalDate

  def addBusinessPeriod(date: LocalDate, period: Tenor)(using BusinessDayConvention): LocalDate

  final def isHoliday(date: LocalDate) = !isBusinessDay(date)

object Calendar:

  def apply(): Calendar = NoHolidaysCalendar

case object NoHolidaysCalendar extends Calendar:

  def isBusinessDay(date: LocalDate): Boolean = true

  def addBusinessPeriod(date: LocalDate, period: Tenor)(using BusinessDayConvention): LocalDate =
    date + period

  def addBusinessDays(date: LocalDate, days: Long): LocalDate =
    date.plusDays(days)
