package lib

import cats.syntax.all.*
import lib.dtos.BusinessDayConvention
import lib.dtos.BusinessDayConvention.*
import lib.quantities.*

trait Calendar[T: DateLike]:

  def isBusinessDay(t: T): Boolean

  final def addBusinessDays(t: T, n: Long): T =
    val step = math.signum(n)
    Iterator
      .iterate((0, t)): (i, curr) =>
        val next = DateLike[T].plusDays(curr, step)
        if !isBusinessDay(curr) then i -> next else i + 1 -> next
      .takeWhile(_(0) <= n)
      .map(_(1))
      .foldLeft(t)((_, t) => t)

  final def adjust(t: T, bdc: BusinessDayConvention): T =
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

  final def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T =
    val unadjusted = DateLike[T].plusPeriod(t, period.toPeriod)
    adjust(unadjusted, summon[BusinessDayConvention])

  final def countBusinessDays(from: T, to: T): Long =
    if from <= to then DateLike[T].daysBetween(from, to).count(isBusinessDay)
    else -DateLike[T].daysBetween(to, from).count(isBusinessDay)

object Calendar:

  def fromHolidays[T: DateLike](holidays: Seq[T]) = new Calendar[T]:
    def isBusinessDay(t: T): Boolean = !holidays.contains(t) && !DateLike[T].isWeekend(t)

  def all[T: DateLike] = new Calendar[T]:
    def isBusinessDay(t: T): Boolean = true
