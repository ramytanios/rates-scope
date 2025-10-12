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

    given Conversion[Double, YearFraction] = YearFraction.apply

  /** Annualized interest rate */
  opaque type Rate = Double

  object Rate:
    def apply(r: Double): Rate = r

    extension (r: Rate)
      def *(yf: YearFraction): Double = r * yf
      def unary_- : Rate = -r

    given Conversion[Double, Rate] = Rate.apply

  /** Tenor */
  opaque type Tenor = Period

  object Tenor:
    def apply(t: Tenor): Period = t

    val `1D`: Tenor = Period.ofDays(1)
    val `3M`: Tenor = Period.ofMonths(3)
    val `6M`: Tenor = Period.ofMonths(6)
    val `1Y`: Tenor = Period.ofYears(1)
    val `2Y`: Tenor = Period.ofYears(2)

    extension (t: Tenor)
      def toPeriod: Period = t

    given Conversion[Period, Tenor] = Tenor.apply
