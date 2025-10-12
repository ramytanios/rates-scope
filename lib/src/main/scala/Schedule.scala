package lib

import java.time.LocalDate
import lib.quantities.*
import lib.syntax.*

object Schedule:

  /** Generated schedule is guaranteed to have `from` and `to` included */
  def generate(
      from: LocalDate,
      to: LocalDate,
      period: Tenor,
      calendar: Calendar,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction
  ): Seq[LocalDate] =

    require(from < to, s"from date $from must be before $to")

    direction match
      case Direction.Backward =>
        val init =
          LazyList
            .iterate(to)(calendar.addBusinessPeriod(_, -period)(using bdConvention))
            .takeWhile(to < _)
            .reverse
            .toSeq

        val withStub =
          stub match
            case StubConvention.Short => init
            case StubConvention.Long =>
              if calendar.addBusinessPeriod(init.head, -period)(using bdConvention) < from then
                init.drop(1)
              else init

        from +: withStub

      case Direction.Forward =>
        val init =
          LazyList
            .iterate(from)(calendar.addBusinessPeriod(_, period)(using bdConvention))
            .takeWhile(_ < to)
            .toSeq

        val withStub =
          stub match
            case StubConvention.Short => init
            case StubConvention.Long =>
              if to < calendar.addBusinessPeriod(init.last, period)(using bdConvention) then
                init.dropRight(1)
              else init

        withStub :+ to

  enum StubConvention:
    case Short
    case Long

  enum Direction:
    case Backward
    case Forward
