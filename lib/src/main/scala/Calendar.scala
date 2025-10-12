package lib

import lib.quantities.*
import lib.syntax.*

import java.time.LocalDate

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
