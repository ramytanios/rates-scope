package lib

import lib.quantities.*

import java.time.LocalDate
import java.time.temporal.ChronoUnit

sealed trait DayCounter:

  def daysBetween(from: LocalDate, to: LocalDate): Long

  def yearFraction(from: LocalDate, to: LocalDate): YearFraction

  final def apply(from: LocalDate, to: LocalDate): YearFraction = yearFraction(from, to)

case object Act360 extends DayCounter:

  override def daysBetween(from: LocalDate, to: LocalDate): Long =
    ChronoUnit.DAYS.between(from, to)

  override def yearFraction(from: LocalDate, to: LocalDate): YearFraction =
    YearFraction(daysBetween(from, to) / 360.0)

case object Act365 extends DayCounter:

  override def daysBetween(from: LocalDate, to: LocalDate): Long =
    ChronoUnit.DAYS.between(from, to)

  override def yearFraction(from: LocalDate, to: LocalDate): YearFraction =
    YearFraction(daysBetween(from, to) / 365.0)
