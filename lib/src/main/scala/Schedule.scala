package lib

import lib.quantities.*
import lib.syntax.given

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayDeque
import scala.math.Ordering.Implicits.*

object Schedule:

  def apply[T: DateLike](
      from: T,
      to: T,
      period: Tenor,
      calendar: Calendar[T],
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction
  ): Vector[T] =

    require(from < to, s"from date $from must be before $to")

    direction match
      case Direction.Backward =>
        val buf = new ArrayDeque[T]
        var loop = true
        var idx = 0
        while loop do
          val curr = calendar.addBusinessPeriod(to, period * idx)(using bdConvention)
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
        val buf = new ArrayBuffer[T]
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
