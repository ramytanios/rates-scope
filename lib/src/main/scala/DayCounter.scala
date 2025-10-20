package lib

import lib.quantities.*

import java.time.temporal.ChronoUnit
import java.time.temporal.Temporal

trait DayCounter[T]:

  def daysBetween(from: T, to: T): Long

  def yearFraction(from: T, to: T): YearFraction

  final def apply(from: T, to: T): YearFraction = yearFraction(from, to)

object DayCounter: // or givens [LocalDate, Act360], [LocalDate, Act365]

  def Act360[T <: Temporal] = new DayCounter[T]:

    override def daysBetween(from: T, to: T): Long =
      ChronoUnit.DAYS.between(from, to)

    override def yearFraction(from: T, to: T): YearFraction =
      YearFraction(daysBetween(from, to) / 360.0)

  def Act365[T <: Temporal] = new DayCounter[T]:

    def daysBetween(from: T, to: T): Long =
      ChronoUnit.DAYS.between(from, to)

    def yearFraction(from: T, to: T): YearFraction =
      YearFraction(daysBetween(from, to) / 365.0)
