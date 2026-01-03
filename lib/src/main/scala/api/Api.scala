package lib.api

import lib.*
import lib.dtos
import lib.quantities.Tenor
import lib.syntax.*

class Api[T: lib.DateLike](val market: Market[T]):

  val l = new Lib(market)
  import l.*

  def price(payoff: dtos.Payoff[T]): Either[lib.Error, Double] =
    payoff match
      case p: dtos.Payoff.Caplet[T] =>
        for
          caplet <- buildCaplet(p)
          rate <- caplet.rate.asRight
          volSurface <- buildVolSurface(caplet.paymentCurrency, rate.tenor)
          fixings <- buildFixings(p.rate)
          price <- caplet.price(market.t, volSurface, fixings)
        yield price

      case p: dtos.Payoff.Swaption[T] =>
        for
          swaption <- buildSwaption(p)
          rate <- swaption.rate.asRight
          volSurface <- buildVolSurface(rate.currency, rate.tenor)
          fixings <- buildFixings(p.rate)
          price <- swaption.price(market.t, volSurface, fixings)
        yield price

      case p: dtos.Payoff.BackwardLookingCaplet[T] =>
        for
          caplet <- buildBackwardLookingCaplet(p)
          volCube <- buildVolCube(caplet.rate.currency)
          fixings <- buildFixings(p.rate)
          price <- caplet.price(market.t, volCube, fixings)
        yield price

  def arbitrageCheck(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor
  ): Either[lib.Error, Option[Arbitrage]] =
    buildVolConventions(currency, tenor).flatMap: rate =>
      val expiryT = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
      buildVolSurface(currency, tenor).map(_(expiryT)).map: volSkew =>
        CDFInverter(market.t, expiryT, volSkew, rate.forward, Params()).swap.toOption

  def sample(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor,
      nSamples: Int
  ): Either[lib.Error, Api.SamplingResult] =
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
              Api.SamplingResult(quotedStrikes, quotedVols, strikes, vols, pdf)

object Api:

  case class SamplingResult(
      quotedStrikes: Seq[Double],
      quotedVols: Seq[Double],
      strikes: Seq[Double],
      vols: Seq[Double],
      pdf: Seq[Double]
  )
