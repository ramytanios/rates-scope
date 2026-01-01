package lib.interface

import lib.DayCounter
import lib.bachelier
import lib.dtos
import lib.quantities.Tenor
import lib.syntax.*

class VolSampler[T: lib.DateLike](val market: Market[T]):

  val interface = new Interface(market)

  import interface.*

  def sample(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor,
      nSamples: Int
  ): Either[lib.Error, VolSampler.Result] =
    market.volSurface(currency, tenor)
      .flatMap: volSurface =>
        volSurface.surface.get(expiry.toPeriod).toRight(lib.Error.Generic(s"Invalid expiry $expiry"))
      .flatMap: volSkew =>
        val (ms, _) = volSkew.skew.unzip
        buildVolConventions(currency, tenor)
          .flatMap: rate =>
            val expiryT =
              rate.calendar.addBusinessPeriod(market.t, expiry.toPeriod)(using rate.bdConvention)
            val fwd = rate.forward(expiryT)
            val quotedStrikes = ms.map(fwd - _)
            buildVolSurface(currency, tenor).map: volSurface =>
              val volSkew = volSurface(expiryT)
              val quotedVols = quotedStrikes.map(volSkew)
              val dt =
                market.t.yearFractionTo(expiryT)(using summon[lib.DateLike[T]], DayCounter.Act365)
              val atmStdv = volSkew(fwd) * math.sqrt(dt.toDouble)
              val kMin = quotedStrikes.head - 3 * atmStdv
              val kMax = quotedStrikes.last + 3 * atmStdv
              val step = (kMax - kMin) / nSamples
              val strikes = (0 to nSamples).map(i => kMin + i * step)
              val vols = strikes.map(volSkew)
              val pdf = strikes.map(bachelier.impliedDensity(
                fwd,
                dt.toDouble,
                volSkew,
                volSkew.fstDerivative,
                volSkew.sndDerivative
              ))
              VolSampler.Result(quotedStrikes, quotedVols, strikes, vols, pdf)

object VolSampler:

  case class Result(
      quotedStrikes: Seq[Double],
      quotedVols: Seq[Double],
      strikes: Seq[Double],
      vols: Seq[Double],
      pdf: Seq[Double]
  )
