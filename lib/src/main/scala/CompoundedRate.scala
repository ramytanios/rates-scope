package lib

import lib.quantities.*

import java.time.LocalDate
import lib.Schedule.StubConvention
import lib.Schedule.Direction

import cats.syntax.all.*
import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint

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
    Schedule
      .generate(from, to, rate.tenor, rate.calendar, rate.bdConvention, stub, direction)
      .sliding(2)
      .collect:
        case Seq(from0, to0) =>
          val fixingDate = rate.settlementRule.fixingDate(from0, rate.calendar)
          CompoundingPeriod(fixingDate, from0, to0)
      .toVector

  val fixingDates = schedule.map(_.fixingDate)

  val firstFixingDate: LocalDate = schedule.head.fixingDate
  val lastFixingDate: LocalDate = schedule.last.fixingDate

  def compoundingFactor(upTo: LocalDate)(using market: Market): Either[MarketError, Double] =
    schedule
      .traverseCollect:
        case CompoundingPeriod(fixingDate, startDate, endDate) if fixingDate <= upTo =>
          market.fixings(rate.name).flatMap: fixings =>
            fixings(fixingDate).map: fixing =>
              (1 + rate.dayCounter.yearFraction(startDate, endDate) * fixing.value)
      .map(_.product)

  def fullCompoundingFactor(using Market) = compoundingFactor(lastFixingDate)

  def forward(using market: Market): Either[Error, Double] =

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
          val obsIdx = fixingDates.search(t) match
            case Found(i)          => i
            case InsertionPoint(i) => i - 1
          val futIdx = obsIdx + 1
          compoundingFactor(schedule(obsIdx).fixingDate).map:
            _ / curve.discount(schedule(futIdx).interestStart, to)
      .map(forwardCompoundingFactor => (forwardCompoundingFactor - 1.0) / dcf.toDouble)
