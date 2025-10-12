package lib

import java.time.LocalDate

trait Calendar:

  def isBusinessDay(date: LocalDate): Boolean

  def addBusinessDays(date: LocalDate, days: Long): LocalDate

  final def isHoliday(date: LocalDate) = !isBusinessDay(date)

object Calendar:

  def apply(): Calendar = NoHolidaysCalendar

case object NoHolidaysCalendar extends Calendar:

  override def isBusinessDay(date: LocalDate): Boolean = true

  override def addBusinessDays(date: LocalDate, days: Long): LocalDate =
    date.plusDays(days)
