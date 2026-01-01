package lib.interface

import cats.syntax.all.*
import lib.dtos

class VanillaPricer[T: lib.DateLike](val market: Market[T]):

  val mapper = new Mapper(market)

  import mapper.*

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
