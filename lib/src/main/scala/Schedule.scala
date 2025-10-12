package lib

import java.time.LocalDate
import lib.quantities.*
import lib.syntax.*

object schedule:

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
    throw new NotImplementedError()

  enum StubConvention:
    case Long
    case Short

  enum Direction:
    case Forward
    case Backward
