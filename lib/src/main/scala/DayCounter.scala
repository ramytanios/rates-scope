package lib

import lib.quantities.*

import java.time.LocalDate
import java.time.temporal.ChronoUnit

trait DayCounter:

  def daysBetween(from: LocalDate, to: LocalDate): Long

  def yearFraction(from: LocalDate, to: LocalDate): YearFraction

object DayCounter:

  val Act360: DayCounter = new DayCounter:

    def daysBetween(from: LocalDate, to: LocalDate): Long =
      ChronoUnit.DAYS.between(from, to)

    def yearFraction(from: LocalDate, to: LocalDate): YearFraction =
      YearFraction(daysBetween(from, to) / 360.0)

  val Act365: DayCounter = new DayCounter:

    def daysBetween(from: LocalDate, to: LocalDate): Long =
      ChronoUnit.DAYS.between(from, to)

    def yearFraction(from: LocalDate, to: LocalDate): YearFraction =
      YearFraction(daysBetween(from, to) / 365.0)
