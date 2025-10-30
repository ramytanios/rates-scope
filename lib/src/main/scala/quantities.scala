package lib

import java.time.Period

object quantities:

  /** Year fraction */
  opaque type YearFraction = Double

  object YearFraction:
    def apply(yf: Double): YearFraction = yf

    val zero: YearFraction = 0.0
    val oneDay: YearFraction = 1.0 / 365.0
    val oneYear: YearFraction = 1.0

    extension (yf: YearFraction)
      def toDouble: Double = yf
      def +(other: YearFraction) = yf + other
      def -(other: YearFraction) = yf - other
      def *(other: Double) = yf * other
      def /(other: YearFraction) = yf / other

    given Conversion[Double, YearFraction] = YearFraction.apply

  /** Annualized interest rate */
  opaque type Rate = Double

  object Rate:
    def apply(r: Double): Rate = r

    extension (r: Rate)
      def toDouble: Double = r
      def *(yf: YearFraction): Double = r * yf
      def unary_- : Rate = -r

    given Conversion[Double, Rate] = Rate.apply

  /** Strike */
  opaque type Strike = Double

  object Strike:
    def apply(r: Double): Rate = r

    extension (r: Strike)
      def toDouble: Double = r

    given Conversion[Double, Strike] = Strike.apply

  /** Tenor */
  opaque type Tenor = Period

  object Tenor:
    def apply(t: Period): Tenor = t

    val `1D`: Tenor = Period.ofDays(1)
    val `3M`: Tenor = Period.ofMonths(3)
    val `6M`: Tenor = Period.ofMonths(6)
    val `1Y`: Tenor = Period.ofYears(1)
    val `2Y`: Tenor = Period.ofYears(2)

    def days(n: Int) = Period.ofDays(n)
    def months(n: Int) = Period.ofMonths(n)
    def years(n: Int) = Period.ofYears(n)

    enum Unit:
      case Day, Week, Month, Year

    def getUnit(t: Tenor): Unit =
      if t.getYears != 0 && t.getMonths == 0 && t.getDays == 0 then Unit.Year
      else if t.getYears == 0 && t.getMonths != 0 && t.getDays == 0 then Unit.Month
      else if t.getYears == 0 && t.getMonths == 0 && t.getDays != 0 && t.getDays % 7 == 0 then
        Unit.Week
      else if t.getYears == 0 && t.getMonths == 0 && t.getDays != 0 then Unit.Day
      else throw RuntimeException("Unable to infer unit")

    extension (t: Tenor)
      def toPeriod: Period = t
      def unary_- : Tenor = t.multipliedBy(-1)
      def *(factor: Int): Tenor = t.multipliedBy(factor)
      def days: Int = t.getDays
      def toYearFraction: YearFraction = t.getDays / 365.0
      def unit: Unit = getUnit(t)

    given Ordering[Tenor] = Ordering.by(_.toYearFraction)

    given Conversion[Period, Tenor] = Tenor.apply
