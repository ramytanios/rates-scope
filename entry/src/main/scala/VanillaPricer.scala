package entry

import cats.syntax.all.*

class VanillaPricer[T: lib.DateLike](val market: Market[T]):

  val builder = new Builder(market)

  def price(payoff: dtos.Payoff[T]): Either[lib.Error, Double] =
    payoff match
      case c: dtos.Payoff.Caplet[T] =>
        builder.caplet(c).flatMap: caplet =>
          builder.volSurface(caplet.paymentCurrency, caplet.rate.tenor, caplet.rate.forward)
            .flatMap: volSurface =>
              caplet.price(market.t, volSurface)
