package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.*
import lib.syntax.{ *, given }
import lib.utils.BinarySearch.Found
import lib.utils.BinarySearch.InsertionLoc

case class CompoundingPeriod[T](fixingDate: T, interestStart: T, interestEnd: T)

class CompoundedRate[T: TimeLike](
    val from: T,
    val to: T,
    val rate: Libor[T],
    val stub: StubConvention,
    val direction: Direction
):

  given DayCounter[T] = rate.dayCounter

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

  def compoundingFactor(toInclusive: T)(using Market[T]): Either[MarketError, Double] =
    schedule
      .traverseCollect:
        case CompoundingPeriod(fixingAt, startAt, endAt) if fixingAt <= toInclusive =>
          summon[Market[T]].fixings(rate.name).flatMap: fixings =>
            fixings(fixingAt).map: fixing =>
              (1 + startAt.yearFractionTo(endAt) * fixing.value)
      .map(_.product)

  def fullCompoundingFactor(using Market[T]) = compoundingFactor(lastFixingDate)

  def forward(using Market[T]): Either[Error, Double] =
    val market = summon[Market[T]]
    val t = market.ref

    Either.raiseWhen(t > lastFixingDate)(
      Error.Generic(s"ref date $t is after last fixing $lastFixingDate")
    )
      .flatMap: _ =>
        market.yieldCurve(rate.resetCurve)
      .flatMap: curve =>
        if t < firstFixingDate then
          (1 / curve.discount(from, to)).asRight
        else if t == lastFixingDate then fullCompoundingFactor
        else
          val obsIdx = schedule.searchBy(_.fixingDate)(t) match
            case Found(i)        => i
            case InsertionLoc(i) => i - 1
          val futIdx = obsIdx + 1
          compoundingFactor(schedule(obsIdx).fixingDate).map:
            _ / curve.discount(schedule(futIdx).interestStart, to)
      .map(forwardCompoundingFactor => (forwardCompoundingFactor - 1.0) / dcf.toDouble)
