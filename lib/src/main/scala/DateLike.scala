package lib

import cats.kernel.Order
import lib.quantities.*

import java.time.DayOfWeek
import java.time.LocalDate
import scala.jdk.CollectionConverters.*

trait DateLike[T] extends Order[T]:

  final def yearFraction(from: T, to: T)(using DayCounter): YearFraction =
    summon[DayCounter].yearFraction(toLocalDate(from), toLocalDate(to))

  // convenience for day count fractions calculations
  // TODO To be revisited
  def toLocalDate(t: T): LocalDate

  def plusPeriod(t: T, period: Tenor): T

  def plusDays(t: T, days: Long): T

  def daysBetween(from: T, to: T): Seq[T]

  def isWeekend(t: T): Boolean

  def onSameMonth(t0: T, t1: T): Boolean

object DateLike:

  def apply[T](using ev: DateLike[T]) = ev

  given DateLike[LocalDate] = new DateLike[LocalDate]:

    def compare(x: LocalDate, y: LocalDate): Int =
      if x.isEqual(y) then 0
      else if x.isBefore(y) then -1
      else 1

    def toLocalDate(t: LocalDate): LocalDate = t

    def plusDays(t: LocalDate, days: Long): LocalDate = t.plusDays(days)

    def plusPeriod(t: LocalDate, period: Tenor): LocalDate = t.plus(period.toPeriod)

    def daysBetween(from: LocalDate, to: LocalDate): Seq[LocalDate] =
      from.datesUntil(to).iterator.asScala.toSeq

    def isWeekend(t: LocalDate): Boolean =
      val day = t.getDayOfWeek()
      day == DayOfWeek.SATURDAY || day == DayOfWeek.SUNDAY

    def onSameMonth(t0: LocalDate, t1: LocalDate): Boolean =
      t0.getMonth == t1.getMonth

  trait Syntax:

    given [T: DateLike]: Ordering[T] = DateLike[T].toOrdering

    extension [T: DateLike](t: T)
      def yearFractionTo(to: T)(using DayCounter) = DateLike[T].yearFraction(t, to)
      def yearFractionFrom(from: T)(using DayCounter) = DateLike[T].yearFraction(from, t)
      def -(other: T) = if DateLike[T].compare(t, other) <= 0 then
        DateLike[T].daysBetween(t, other).size
      else -DateLike[T].daysBetween(other, t).size
