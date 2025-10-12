package lib

import lib.quantities.*

import java.time.LocalDate
import lib.Schedule.StubConvention
import lib.Schedule.Direction

import cats.syntax.all.*

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
) extends Underlying:

  val dcf: YearFraction = rate.dayCounter.yearFraction(from, to)

  private val schedule: IndexedSeq[CompoundingPeriod] =
    Schedule
      .generate(from, to, rate.tenor, rate.calendar, rate.bdConvention, stub, direction)
      .sliding(2)
      .collect:
        case Seq(_start, _end) =>
          val fixingDate = rate.settlementRule.fixingDate(_start, rate.calendar)
          CompoundingPeriod(fixingDate, _start, _end)
      .toIndexedSeq

  def forward(t: LocalDate)(using market: Market): Either[Error, Double] =
    Either.raiseUnless(t.isEqual(market.ref))(
      new Error.Generic("computing forward of a compounded rate only makes sense at the ref date!")
    ).as:
      0.0
