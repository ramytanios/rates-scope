package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.*
import lib.syntax.{ *, given }
import lib.utils.BinarySearch.Found
import lib.utils.BinarySearch.InsertionLoc

case class CompoundingPeriod[T](
    fixingAt: T,
    startAt: T, // interest period start
    endAt: T // interest period end
)

class CompoundedRate[T: DateLike](
    val rate: Libor[T],
    val schedule: Vector[CompoundingPeriod[T]]
):

  given DayCounter = rate.dayCounter

  val from = schedule.head.startAt
  val to = schedule.last.endAt

  val dcf: YearFraction = from.yearFractionTo(to)

  val firstFixingAt = schedule.head.fixingAt
  val lastFixingAt = schedule.last.fixingAt

  def compoundingFactor(toInclusive: T, fixings: Map[T, Double]): Double =
    schedule.collect:
      case CompoundingPeriod(fixingAt, startAt, endAt) if fixingAt <= toInclusive =>
        val fixing = fixings(fixingAt)
        (1 + startAt.yearFractionTo(endAt) * fixing)
    .product

  def fullCompoundingFactor(fixings: Map[T, Double]) = compoundingFactor(lastFixingAt, fixings)

  def findObservationIdx(t: T): Int =
    schedule.searchBy(_.fixingAt)(t) match
      case Found(i)        => i
      case InsertionLoc(i) => i - 1

  def forward(t: T, fixings: Map[T, Double]): Either[Error, Double] =
    Either.raiseWhen(t > lastFixingAt)(
      Error.Generic(s"$t is after last fixing $lastFixingAt")
    ).map: _ =>
      if t < firstFixingAt then
        1.0 / rate.resetCurve.discount(from, to)
      else if t == lastFixingAt then fullCompoundingFactor(fixings)
      else
        val obsIdx = findObservationIdx(t)
        val futIdx = obsIdx + 1
        compoundingFactor(schedule(obsIdx).fixingAt, fixings) /
          rate.resetCurve.discount(schedule(futIdx).startAt, to)
    .map: f =>
      (f - 1.0) / dcf.toDouble

object CompoundedRate:

  def apply[T: DateLike](
      from: T,
      to: T,
      rate: Libor[T],
      stub: StubConvention,
      direction: Direction
  ): CompoundedRate[T] =

    val schedule: Vector[CompoundingPeriod[T]] =
      Schedule(from, to, rate.tenor, rate.calendar, rate.bdConvention, stub, direction)
        .sliding(2)
        .collect:
          case Seq(t0, t1) =>
            val fixingDate = rate.settlementRule.fixingDate(t0)
            CompoundingPeriod(fixingDate, t0, t1)
        .toVector

    new CompoundedRate[T](rate, schedule)
