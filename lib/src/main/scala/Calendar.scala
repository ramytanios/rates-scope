package lib

import lib.quantities.*

trait Calendar[T]:

  def isBusinessDay(t: T): Boolean

  def addBusinessDays(t: T, days: Long): T

  def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T

  final def isHoliday(t: T) = !isBusinessDay(t)

object Calendar:

  def apply[T: TimeLike](): Calendar[T] = noHolidays[T]

  def noHolidays[T: TimeLike] = new Calendar[T]:

    def isBusinessDay(t: T): Boolean = true

    def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T =
      TimeLike[T].plusPeriod(t, period.toPeriod)

    def addBusinessDays(t: T, days: Long): T =
      TimeLike[T].plusDays(t, days)
