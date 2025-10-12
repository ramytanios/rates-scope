package lib

import scala.math.*

object bachelier:

  import normal.*

  def price(
      ot: OptionType,
      forward: Double,
      strike: Double,
      tte: Double,
      vol: Double,
      df: Double
  ): Double =

    require(tte >= 0.0 && vol >= 0.0, "tte and vol must be positive")

    val call =
      if tte == 0 || vol == 0.0 then
        df * max(forward - strike, 0.0)
      else
        val stdv = vol * sqrt(tte)
        val d = (forward - strike) / stdv
        df * ((forward - strike) * cdf(d) + stdv * pdf(d))

    ot match
      case OptionType.Call => call
      case OptionType.Put  => call - df * (forward - strike)
