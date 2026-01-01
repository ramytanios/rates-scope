package lib.interface

import lib.Arbitrage
import lib.CDFInverter
import lib.Params
import lib.dtos
import lib.quantities.Tenor

class ArbitrageChecker[T: lib.DateLike](val market: Market[T]):

  val interface = new Interface(market)

  import interface.*

  def arbitrageCheck(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor
  ): Either[lib.Error, Option[Arbitrage]] =
    buildVolConventions(currency, tenor).flatMap: rate =>
      val expiryT = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
      buildVolSurface(currency, tenor).map(_(expiryT)).map: volSkew =>
        CDFInverter(market.t, expiryT, volSkew, rate.forward, Params()).swap.toOption
