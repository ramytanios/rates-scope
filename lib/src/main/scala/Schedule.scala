package lib

import lib.quantities.*

import java.time.LocalDate
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayDeque
import scala.math.Ordering.Implicits.*

object Schedule:

  def apply(
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
        val buf = new ArrayDeque[LocalDate]
        var loop = true
        var idx = 0
        while loop do
          val curr = calendar.addBusinessPeriod(from, -period * idx)(using bdConvention)
          if curr > from then
            curr +=: buf
            idx -= 1
          else
            loop = false

        val withStub = stub match
          case StubConvention.Short => buf
          case StubConvention.Long  => buf.dropRight(1)

        (from +=: withStub).toVector

      case Direction.Forward =>
        val buf = new ArrayBuffer[LocalDate]
        var loop = true
        var idx = 0
        while loop do
          val curr = calendar.addBusinessPeriod(from, period * idx)(using bdConvention)
          if curr < to then
            buf += curr
            idx += 1
          else
            loop = false

        val withStub = stub match
          case StubConvention.Short => buf
          case StubConvention.Long  => buf.dropRight(1)

        (withStub += to).toVector

  enum StubConvention:
    case Short
    case Long

  enum Direction:
    case Backward
    case Forward
