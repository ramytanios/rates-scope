package lib

import java.time.LocalDate
import lib.quantities.*

sealed trait DayCounter:

  def daysBetween(from: LocalDate, to: LocalDate): Long

  def yearFraction(from: LocalDate, to: LocalDate): YearFraction

case object Act360 extends DayCounter:

  override def daysBetween(from: LocalDate, to: LocalDate): Long = days(from, to)

  override def yearFraction(from: LocalDate, to: LocalDate): YearFraction =
    YearFraction(days(from, to) / 360.0)

case object Act365 extends DayCounter:

  override def daysBetween(from: LocalDate, to: LocalDate): Long = days(from, to)

  override def yearFraction(from: LocalDate, to: LocalDate): YearFraction =
    YearFraction(days(from, to) / 365.0)
