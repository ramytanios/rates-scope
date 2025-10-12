package lib

import java.time.LocalDate

sealed trait DayCounter:

  def daysBetween(from: LocalDate, to: LocalDate): Long

  def yearFraction(from: LocalDate, to: LocalDate): Double

case object Act360 extends DayCounter:

  override def daysBetween(from: LocalDate, to: LocalDate): Long = days(from, to)

  override def yearFraction(from: LocalDate, to: LocalDate): Double = days(from, to) / 360.0

case object Act365 extends DayCounter:

  override def daysBetween(from: LocalDate, to: LocalDate): Long = days(from, to)

  override def yearFraction(from: LocalDate, to: LocalDate): Double = days(from, to) / 365.0
