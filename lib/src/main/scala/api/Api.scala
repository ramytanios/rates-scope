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

  def sampleVolSkew(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor,
      nSamples: Int,
      nStdvs: Int
  ): Either[lib.Error, Api.SamplingResult] =
    market.volCube(currency).map(_.unit).flatMap: volUnit =>
      val volInUnit = volUnit match
        case dtos.VolUnit.BpPerYear => (v: Double) => v * 10000
      buildVolConventions(currency, tenor).flatMap: rate =>
        given dtos.BusinessDayConvention = rate.bdConvention
        val expiryT = rate.calendar.addBusinessPeriod(market.t, expiry.toPeriod)
        val fwd = rate.forward(expiryT)
        buildVolCube(currency).map: volCube =>
          val volSkew = volCube(tenor)(expiryT)
          val quotedStrikes =
            market.volSurface(currency, tenor).toOption.flatMap(_.surface.get(tenor.toPeriod))
              .map(_.skew.unzip._1.map(_ + fwd)).orEmpty
          val quotedVols = quotedStrikes.map(volSkew andThen volInUnit)
          val dt = market.t.yearFractionTo(expiryT)(using lib.DateLike[T], DayCounter.Act365)
          val atmStdv = volSkew(fwd) * math.sqrt(dt.toDouble)
          val kMin = fwd - nStdvs * atmStdv
          val kMax = fwd + nStdvs * atmStdv
          val step = (kMax - kMin) / nSamples
          val strikes = (0 to nSamples).map(i => kMin + i * step)
          val vols = strikes.map(volSkew andThen volInUnit)
          val impliedPdf = bachelier.impliedDensity(
            fwd,
            dt.toDouble,
            volSkew,
            volSkew.fstDerivative,
            volSkew.sndDerivative
          )
          val pdf = strikes.map(impliedPdf)
          val quotedPdf = quotedStrikes.map(impliedPdf)
          Api.SamplingResult(quotedStrikes, quotedVols, quotedPdf, strikes, vols, pdf)

object Api:

  case class SamplingResult(
      quotedStrikes: Seq[Double],
      quotedVols: Seq[Double],
      quotedPdf: Seq[Double],
      strikes: Seq[Double],
      vols: Seq[Double],
      pdf: Seq[Double]
  )
