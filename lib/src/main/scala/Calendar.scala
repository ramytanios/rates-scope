package lib

import cats.syntax.all.*
import lib.dtos.BusinessDayConvention
import lib.dtos.BusinessDayConvention.*
import lib.quantities.*
import lib.syntax.*

trait Calendar[T: DateLike]:

  def isBusinessDay(t: T): Boolean

  final def addBusinessDays(t: T, days: Int): T =
    val step = math.signum(days)
    var curr = t
    var n = 0
    while n < days do
      val next = curr + step
      curr = next
      if isBusinessDay(next) then
        n += 1
    curr

  private def adjust(t: T, bdc: BusinessDayConvention): T =
    bdc match
      case Following =>
        var res = t; while !isBusinessDay(res) do res = res + 1; res
      case Preceding =>
        var res = t; while !isBusinessDay(res) do res = res - 1; res
      case ModifiedFollowing =>
        val tn = adjust(t, Following)
        if DateLike[T].onSameMonth(t, tn) then tn else adjust(t - 1, Preceding)

  final def addBusinessPeriod(t: T, period: Tenor)(using BusinessDayConvention): T =
    period.unit match
      case Tenor.Unit.Day => addBusinessDays(t, period.toPeriod.getDays)
      case Tenor.Unit.Week =>
        val bdConvention = summon[BusinessDayConvention] match
          case ModifiedFollowing => Following
          case other             => other
        adjust(t + period, bdConvention)
      case _ => adjust(t + period, summon[BusinessDayConvention])

  final def countBusinessDays(t0: T, t1: T): Long =
    if t0 <= t1 then t0.daysTo(t1).count(isBusinessDay)
    else -t1.daysTo(t0).count(isBusinessDay)

object Calendar:

  def fromHolidays[T: DateLike](holidays: Seq[T]): Calendar[T] = new Calendar[T]:
    def isBusinessDay(t: T): Boolean = !holidays.contains(t) && !t.isWeekend

  def all[T: DateLike]: Calendar[T] = new Calendar[T]:
    def isBusinessDay(t: T): Boolean = true
