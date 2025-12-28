package lib

import lib.syntax.*
import org.apache.commons.math3.distribution.NormalDistribution

trait CDFInverse:

  def apply(cdf: Double): Double

enum Arbitrage:
  case LeftAsymptotic
  case RightAsymptotic
  case Density(kl: Double, kr: Double)

case class Params(
    n: Int = 100, // middle grid size
    m: Int = 15, // left & right tails max size
    eps: Double = 0.01 // tail cdf theshold
)

object CDFInverter:

  def apply[T: DateLike](
      t: T,
      expiry: T,
      vol: VolatilitySkew,
      forward: Forward[T],
      params: Params = Params()
  ): Either[Arbitrage, CDFInverse] =

    val dt = t.yearFractionTo(expiry)(using DateLike[T], DayCounter.Act365).toDouble
    val f = forward(expiry)

    val atmStdv = vol(f) * math.sqrt(dt)

    // φ⁻¹ of N(F,σ²T)
    val cdfInvN = NormalDistribution(f, atmStdv).inverseCumulativeProbability

    // cdf implied from vol
    val cdfImplied = bachelier.impliedCumulative(f, dt, vol.apply, vol.fstDerivative)

    // middle strikes generation and arbitrage check
    val middleStrikes =
      val strikes = (0 to params.n - 1).map(i => cdfInvN(i / (params.n - 1)))
      val cdfs = strikes.map(cdfImplied)
      (0 to params.n - 1)
        .find(i => cdfs(i) < cdfs(i - 1))
        .map(i => Arbitrage.Density(strikes(i - 1), strikes(i)))
        .toLeft(strikes -> cdfs)

    ???
