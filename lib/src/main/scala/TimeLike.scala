package lib

import lib.quantities.*

import java.time.LocalDate
import cats.kernel.Order

trait TimeLike[T] extends Order[T]:

  final def apply(from: T, to: T)(using DayCounter): YearFraction = this.yearFraction(from, to)

  def yearFraction(from: T, to: T)(using DayCounter): YearFraction

  def daysBetween(from: T, to: T)(using DayCounter): Long

object TimeLike:

  def apply[T](using ev: TimeLike[T]) = ev

  given [T: TimeLike]: Ordering[T] = TimeLike[T].toOrdering

  given TimeLike[LocalDate] = new TimeLike[LocalDate]:

    def compare(x: LocalDate, y: LocalDate): Int =
      if x.isEqual(y) then 0
      else if x.isBefore(y) then -1
      else 1

    def daysBetween(from: LocalDate, to: LocalDate)(using DayCounter): Long =
      summon[DayCounter].daysBetween(from, to)

    def yearFraction(from: LocalDate, to: LocalDate)(using DayCounter): YearFraction =
      summon[DayCounter].yearFraction(from, to)

extension [T: TimeLike](t: T)
  def yearFractionTo(to: T)(using DayCounter) = TimeLike[T].yearFraction(t, to)
  def yearFractionFrom(from: T)(using DayCounter) = TimeLike[T].yearFraction(from, t)
