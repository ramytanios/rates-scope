package lib

import java.time.LocalDate
import lib.quantities.*

import scala.math.Ordering.Implicits.*

object Schedule:

  def generate(
      from: LocalDate,
      to: LocalDate,
      period: Tenor,
      calendar: Calendar,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction
  ): Vector[LocalDate] =

    require(from < to, s"from date $from must be before $to")

    direction match
      case Direction.Backward =>
        val init = Iterator
          .from(0, -1)
          .map(i => calendar.addBusinessPeriod(from, -period * i)(using bdConvention))
          .takeWhile(to < _)
          .toVector

        val withStub = stub match
          case StubConvention.Short => init
          case StubConvention.Long  => init.dropRight(1)

        (from +: withStub).reverse

      case Direction.Forward =>
        val init = Iterator
          .from(0)
          .map(i => calendar.addBusinessPeriod(from, period * i)(using bdConvention))
          .takeWhile(_ < to)
          .toVector

        val withStub = stub match
          case StubConvention.Short => init
          case StubConvention.Long  => init.dropRight(1)

        (withStub :+ to).toSeq

  enum StubConvention:
    case Short
    case Long

  enum Direction:
    case Backward
    case Forward
