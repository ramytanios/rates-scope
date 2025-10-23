package lib

import lib.quantities.*
import cats.syntax.all.*

trait Calendar[T]:

  def isBusinessDay(t: T): Boolean

  def addBusinessDays(t: T, days: Long): T

  def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T

  def countBusinessDays(from: T, to: T): Long

  final def isHoliday(t: T) = !isBusinessDay(t)

object Calendar:

  def apply[T: DateLike](): Calendar[T] = noHolidays[T]

  def noHolidays[T: DateLike] = new Calendar[T]:

    def isBusinessDay(t: T): Boolean = true

    def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T =
      DateLike[T].plusPeriod(t, period.toPeriod)

    def addBusinessDays(t: T, days: Long): T =
      DateLike[T].plusDays(t, days)

    def countBusinessDays(from: T, to: T): Long =
      if from <= to then DateLike[T].daysBetween(from, to).count(isBusinessDay)
      else -DateLike[T].daysBetween(to, from).count(isBusinessDay)
