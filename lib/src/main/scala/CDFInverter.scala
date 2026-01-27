package lib

import lib.syntax.*
import org.apache.commons.math3.distribution.NormalDistribution

enum Arbitrage:
  case LeftAsymptotic
  case RightAsymptotic
  case Density(leftStrike: Double, rightStrike: Double)

case class Params(
    nMiddle: Int = 500, // number of interior strikes
    nTail: Int = 50, // number strikes per tail
    nTailMax: Int = 15, // tail strikes max iterations
    cdfThreshold: Double = 0.001, // tail cdf theshold
    relPriceThreshold: Double = 0.05 // call put relative price threshold
)

object CDFInverter:

  def apply[T: DateLike](
      t: T,
      expiry: T,
      vol: VolatilitySkew,
      forward: Forward[T],
      params: Params = Params()
  ): Either[Arbitrage, Double => Double] =

    val dt = t.yearFractionTo(expiry)(using DateLike[T], DayCounter.Act365).toDouble

    val fwd = forward(expiry)

    val atmStdv = vol(fwd) * math.sqrt(dt)

    // φ⁻¹ of N(F,σ²T)
    val cdfInvN = NormalDistribution(fwd, atmStdv).inverseCumulativeProbability

    // cdf implied from vol
    val cdfImplied = bachelier.impliedCumulative(fwd, dt, vol.apply, vol.fstDerivative)

    def middleStrikes =
      val strikes = (1 to params.nMiddle).map(i => cdfInvN(i / (params.nMiddle + 1.0)))
      val cdfs = strikes.map(cdfImplied)
      (strikes -> cdfs).asRight[Arbitrage]

    def leftStrikes(kL: Double) =
      if cdfImplied(kL) <= params.cdfThreshold then
        (IndexedSeq.empty[Double], IndexedSeq.empty).asRight[Arbitrage]
      else
        val ks = Iterator.iterate((0, kL))((n, k) => (n + 1, k - atmStdv))
          .takeWhile((n, k) => n <= params.nTailMax && cdfImplied(k) > params.cdfThreshold)
          .map(_(1))
          .drop(1)
          .toIndexedSeq
        Either.raiseWhen(
          ks.size == params.nTailMax && cdfImplied(ks.last) > params.cdfThreshold
        )(Arbitrage.LeftAsymptotic)
          .flatMap: _ =>
            val kmMin = cdfInvN(1.0 / (params.nMiddle + 1.0))
            val kMin = ks.headOption.getOrElse(kmMin)
            val kMax = ks.lastOption.getOrElse(kmMin)
            val put = bachelier.price(dtos.OptionType.Put, fwd, kMin, dt, vol(kMin), 1.0)
            val putAtm = bachelier.price(dtos.OptionType.Put, fwd, fwd, dt, vol(fwd), 1.0)
            Either.raiseUnless(put <= params.relPriceThreshold * putAtm)(Arbitrage.LeftAsymptotic)
              .as:
                val step = (kMax - kMin) / params.nTail
                val strikes = (0 to params.nTail).map(i => kMin + i * step)
                val cdfs = strikes.map(cdfImplied)
                strikes.reverse -> cdfs.reverse

    def rightStrikes(kR: Double) =
      if cdfImplied(kR) <= (1 - params.cdfThreshold) then
        (IndexedSeq.empty[Double], IndexedSeq.empty).asRight[Arbitrage]
      else
        val ks = Iterator.iterate((1, kR))((n, k) => (n + 1, k + atmStdv))
          .takeWhile((n, k) => n <= params.nTailMax && cdfImplied(k) < (1 - params.cdfThreshold))
          .map(_(1))
          .drop(1)
          .toIndexedSeq
        Either.raiseWhen(
          ks.size == params.nTailMax && cdfImplied(ks.last) < (1 - params.cdfThreshold)
        )(Arbitrage.RightAsymptotic)
          .flatMap: _ =>
            val kmMax = cdfInvN(params.nMiddle / (params.nMiddle + 1.0))
            val kMin = ks.headOption.getOrElse(kmMax)
            val kMax = ks.headOption.getOrElse(kmMax)
            val call = bachelier.price(dtos.OptionType.Call, fwd, kMax, dt, vol(kMax), 1.0)
            val callAtm = bachelier.price(dtos.OptionType.Call, fwd, fwd, dt, vol(fwd), 1.0)
            Either.raiseUnless(
              call <= params.relPriceThreshold * callAtm
            )(Arbitrage.RightAsymptotic)
              .as:
                val step = (kMax - kMin) / params.nTail
                val strikes = (0 to params.nTail).map(i => kMin + i * step)
                val cdfs = strikes.map(cdfImplied)
                strikes -> cdfs

    for
      (mks, mvs) <- middleStrikes
      (lks, lvs) <- leftStrikes(mks.head)
      (rks, rvs) <- rightStrikes(mks.last)
      ks = lks ++ mks ++ rks
      vs = lvs ++ mvs ++ rvs
      _ <- vs.indices.init.find(i => vs(i) >= vs(i + 1)).toLeft(())
        .leftMap(i => Arbitrage.Density(ks(i), ks(i + 1)))
    yield LinearInterpolation.withLinearExtrapolation(vs, ks)
