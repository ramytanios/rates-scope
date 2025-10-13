package lib

import scala.math.*

object bachelier:

  import normal.*

  def price(
      optionType: OptionType,
      forward: Double,
      strike: Double,
      timeToExpiry: Double,
      vol: Double,
      discountFactor: Double
  ): Double =

    require(timeToExpiry >= 0.0 && vol >= 0.0, "tte and vol must be positive")

    val call =
      if timeToExpiry == 0 || vol == 0.0 then
        discountFactor * max(forward - strike, 0.0)
      else
        val stdv = vol * sqrt(timeToExpiry)
        val d = (forward - strike) / stdv
        discountFactor * ((forward - strike) * cdf(d) + stdv * pdf(d))

    optionType match
      case OptionType.Call => call
      case OptionType.Put  => call - discountFactor * (forward - strike)

  def impliedCumulative(
      forward: Double,
      tte: Double,
      vol: Double => Double,
      dvol: Double => Double
  ): Double => Double =
    (k: Double) =>
      val d = (forward - k) / sqrt(tte) / vol(k)
      1 - cdf(d) + sqrt(tte) * dvol(k) * pdf(d)

  def impliedDensity(
      forward: Double,
      tte: Double,
      vol: Double => Double,
      dvol: Double => Double,
      ddvol: Double => Double
  ): Double => Double =
    (k: Double) =>
      val d = (forward - k) / sqrt(tte) / vol(k)
      pdf(d) / sqrt(tte) / vol(k) * (tte * vol(k) * ddvol(k) + pow(
        (1 + (forward - k) * dvol(k) / vol(k)),
        2
      ))
