package lib

import cats.syntax.all.*
import lib.BusinessDayConvention.*
import lib.quantities.*

trait Calendar[T]:

  def isBusinessDay(t: T): Boolean

  def addBusinessDays(t: T, n: Long): T

  def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T

  def countBusinessDays(from: T, to: T): Long

object Calendar:

  def apply[T: DateLike](): Calendar[T] = noHolidays[T]

  def weekends[T: DateLike] = new Calendar[T]:

    def isBusinessDay(t: T): Boolean = !DateLike[T].isWeekend(t)

    def addBusinessDays(t: T, n: Long): T =
      val step = math.signum(n)
      Iterator
        .iterate((0, t)): (i, curr) =>
          val next = DateLike[T].plusDays(curr, step)
          if DateLike[T].isWeekend(curr) then i -> next else i + 1 -> next
        .takeWhile(_(0) <= n)
        .map(_(1))
        .foldLeft(t)((_, t) => t)

    def adjust(t: T, bdc: BusinessDayConvention): T =
      bdc match
        case Following =>
          Iterator
            .iterate(t)(DateLike[T].plusDays(_, 1))
            .find(isBusinessDay)
            .get
        case Preceding =>
          Iterator
            .iterate(t)(DateLike[T].plusDays(_, -1))
            .find(isBusinessDay)
            .get
        case ModifiedFollowing =>
          val tn = adjust(t, Following)
          if DateLike[T].onSameMonth(t, tn) then tn else adjust(t, Preceding)

    def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T =
      val unadjusted = DateLike[T].plusPeriod(t, period.toPeriod)
      adjust(unadjusted, summon[BusinessDayConvention])

    def countBusinessDays(from: T, to: T): Long =
      if from <= to then DateLike[T].daysBetween(from, to).count(isBusinessDay)
      else -DateLike[T].daysBetween(to, from).count(isBusinessDay)

  def noHolidays[T: DateLike] = new Calendar[T]:

    def isBusinessDay(t: T): Boolean = true

    def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T =
      DateLike[T].plusPeriod(t, period.toPeriod)

    def addBusinessDays(t: T, n: Long): T =
      DateLike[T].plusDays(t, n)

    def countBusinessDays(from: T, to: T): Long =
      if from <= to then DateLike[T].daysBetween(from, to).count(isBusinessDay)
      else -DateLike[T].daysBetween(to, from).count(isBusinessDay)
