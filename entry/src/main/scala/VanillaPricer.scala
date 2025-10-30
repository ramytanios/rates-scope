package entry

import cats.syntax.all.*

class VanillaPricer[T: lib.DateLike](val market: Market[T]):

  val builder = new Builder(market)

  import builder.*

  def price(payoff: dtos.Payoff[T]): Either[lib.Error, Double] =
    payoff match
      case _caplet: dtos.Payoff.Caplet[T] =>
        for
          caplet <- buildCaplet(_caplet)
          rate <- caplet.rate.asRight
          marketRate <- buildMarketRate(rate.currency, rate.tenor)
          volSurface <- buildVolSurface(caplet.paymentCurrency, rate.tenor, marketRate.forward)
          price <- caplet.price(market.t, volSurface)
        yield price

      case _swaption: dtos.Payoff.Swaption[T] =>
        for
          swaption <- buildSwaption(_swaption)
          rate <- swaption.rate.asRight
          marketRate <- buildMarketRate(rate.currency, rate.tenor)
          volSurface <- buildVolSurface(rate.currency, rate.tenor, marketRate.forward)
          price <- swaption.price(market.t, volSurface)
        yield price

      case _caplet: dtos.Payoff.BackwardLookingCaplet[T] =>
        for
          caplet <- buildBackwardLookingCaplet(_caplet)
          rate <- caplet.rate.asRight
          volCube <- buildVolCube(caplet.rate.currency)
          fixings <- buildFixings(_caplet.rate)
          price <- caplet.price(market.t, volCube, fixings)
        yield price
