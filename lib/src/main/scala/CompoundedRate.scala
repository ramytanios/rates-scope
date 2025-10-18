package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.*
import lib.utils.BinarySearch.Found
import lib.utils.BinarySearch.InsertionLoc

import java.time.LocalDate
import scala.math.Ordering.Implicits.*

case class CompoundingPeriod(
    fixingDate: LocalDate,
    interestStart: LocalDate,
    interestEnd: LocalDate
)

class CompoundedRate(
    val from: LocalDate,
    val to: LocalDate,
    val rate: Libor,
    val stub: StubConvention,
    val direction: Direction
):

  val dcf: YearFraction = rate.dayCounter.yearFraction(from, to)

  val schedule: Vector[CompoundingPeriod] =
    Schedule(from, to, rate.tenor, rate.calendar, rate.bdConvention, stub, direction)
      .sliding(2)
      .collect:
        case Seq(t0, t1) =>
          val fixingDate = rate.settlementRule.fixingDate(t0)
          CompoundingPeriod(fixingDate, t0, t1)
      .toVector

  val firstFixingDate: LocalDate = schedule.head.fixingDate

  val lastFixingDate: LocalDate = schedule.last.fixingDate

  def compoundingFactor(toInclusive: LocalDate)(using Market): Either[MarketError, Double] =
    schedule
      .traverseCollect:
        case CompoundingPeriod(fixingDate, startDate, endDate) if fixingDate <= toInclusive =>
          summon[Market].fixings(rate.name).flatMap: fixings =>
            fixings(fixingDate).map: fixing =>
              (1 + rate.dayCounter.yearFraction(startDate, endDate) * fixing.value)
      .map(_.product)

  def fullCompoundingFactor(using Market) = compoundingFactor(lastFixingDate)

  def forward(using Market): Either[Error, Double] =
    val market = summon[Market]
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
