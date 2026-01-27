package lib

import lib.dtos.BusinessDayConvention
import lib.dtos.BusinessDayConvention.*
import lib.quantities.*
import lib.syntax.*
import lib.syntax.given

import scala.collection.Searching.Found

trait Calendar[T: DateLike]:

  def isBusinessDay(t: T): Boolean

  final def addBusinessDays(t: T, days: Int): T =
    val step = math.signum(days)
    var curr = t
    var n = 0
    while n < days.abs do
      val next = curr + step
      curr = next
      if isBusinessDay(next) then
        n += 1
    curr

  final def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T =
    period.unit match
      case Tenor.Unit.Day => addBusinessDays(t, period.days)
      case Tenor.Unit.Week =>
        val bdConvention = summon[BusinessDayConvention] match
          case ModifiedFollowing => Following
          case other             => other
        bdConvention.adjust(t + period, this)
      case _ => summon[BusinessDayConvention].adjust(t + period, this)

  final def countBusinessDays(t0: T, t1: T): Long =
    if t0 <= t1 then t0.daysTo(t1).count(isBusinessDay)
    else -t1.daysTo(t0).count(isBusinessDay)

object Calendar:

  // TODO incoherent weekends logic, to be revisited
  def fromHolidays[T: DateLike](holidays: IndexedSeq[T]): Calendar[T] =
    require(
      holidays.isEmpty || (holidays.nonEmpty && holidays.isStrictlyIncreasing),
      "holidays must be strictly increasing"
    )
    new Calendar[T]:
      def isBusinessDay(t: T): Boolean =
        val isHoliday = holidays.search(t) match
          case Found(_) => true
          case _        => false
        !isHoliday && !t.isWeekend

  def all[T: DateLike]: Calendar[T] = new Calendar[T]:
    def isBusinessDay(t: T): Boolean = true
