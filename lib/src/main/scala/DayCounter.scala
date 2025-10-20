package lib

import lib.quantities.*

trait DayCounter[T]:

  def yearFraction(from: T, to: T): YearFraction

object DayCounter:

  enum Method:
    case Act360, Act365

  def apply[T: TimeLike](method: Method): DayCounter[T] =
    method match
      case Method.Act360 => Act360[T]()
      case Method.Act365 => Act365[T]()

  def Act360[T: TimeLike](): DayCounter[T] = new DayCounter[T]:

    def yearFraction(from: T, to: T): YearFraction =
      YearFraction(TimeLike[T].daysBetween(from, to) / 360.0)

  def Act365[T: TimeLike](): DayCounter[T] = new DayCounter[T]:

    def yearFraction(from: T, to: T): YearFraction =
      YearFraction(TimeLike[T].daysBetween(from, to) / 365.0)
