package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.*
import lib.syntax.{ *, given }
import lib.utils.BinarySearch.Found
import lib.utils.BinarySearch.InsertionLoc

case class CompoundingPeriod[T](fixingDate: T, interestStart: T, interestEnd: T)

class CompoundedRate[T: DateLike](
    val from: T,
    val to: T,
    val rate: Libor[T],
    val stub: StubConvention,
    val direction: Direction
):

  given DayCounter = rate.dayCounter

  val dcf: YearFraction = from.yearFractionTo(to)

  val schedule: Vector[CompoundingPeriod[T]] =
    Schedule(from, to, rate.tenor, rate.calendar, rate.bdConvention, stub, direction)
      .sliding(2)
      .collect:
        case Seq(t0, t1) =>
          val fixingDate = rate.settlementRule.fixingDate(t0)
          CompoundingPeriod(fixingDate, t0, t1)
      .toVector

  val firstFixingDate = schedule.head.fixingDate
  val lastFixingDate = schedule.last.fixingDate

  def compoundingFactor(toInclusive: T, fixings: Map[T, Fixing[T]]): Double =
    schedule.collect:
      case CompoundingPeriod(fixingAt, startAt, endAt) if fixingAt <= toInclusive =>
        val fixing = fixings(fixingAt)
        (1 + startAt.yearFractionTo(endAt) * fixing.value)
    .product

  def fullCompoundingFactor(fixings: Map[T, Fixing[T]]) = compoundingFactor(lastFixingDate, fixings)

  def forward(t: T, fixings: Map[T, Fixing[T]]): Either[Error, Double] =

    Either.raiseWhen(t > lastFixingDate)(
      Error.Generic(s"ref date $t is after last fixing $lastFixingDate")
    ).map: _ =>
      if t < firstFixingDate then
        1.0 / rate.resetCurve.discount(from, to)
      else if t == lastFixingDate then fullCompoundingFactor(fixings)
      else
        val obsIdx = schedule.searchBy(_.fixingDate)(t) match
          case Found(i)        => i
          case InsertionLoc(i) => i - 1
        val futIdx = obsIdx + 1
        val f = compoundingFactor(schedule(obsIdx).fixingDate, fixings) /
          rate.resetCurve.discount(schedule(futIdx).interestStart, to)
        (f - 1.0) / dcf.toDouble
