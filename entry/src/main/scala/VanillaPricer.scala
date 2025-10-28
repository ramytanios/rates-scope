package entry

import cats.syntax.all.*
import entry.data.*
import entry.data.Payoff.*
import entry.data.Underlying.*

class VanillaPricer[T: lib.DateLike](val market: Market[T]):

  def price(payoff: data.Payoff[T]): Either[lib.Error, Double] =
    payoff match
      case Caplet(
            rate,
            fixingAt,
            startAt,
            endAt,
            paymentAt,
            paymentCurrency,
            strike,
            discountCurve,
            optionType
          ) =>
        market.rate(rate).flatMap: _underlying =>
          market.yieldCurve(discountCurve).flatMap: _discountCurve =>
            val yieldCurve = lib.YieldCurve(market.t, _discountCurve.discounts.toIndexedSeq)
            _underlying match
              case Libor(
                    name,
                    currency,
                    tenor,
                    spotLag,
                    dayCounter,
                    calendar,
                    resetCurve,
                    bdConvention
                  ) =>
                market.yieldCurve(resetCurve).map: _resetCurve =>
                  val resetCurve = lib.YieldCurve(market.t, _resetCurve.discounts.toIndexedSeq)
                  val libor = new lib.Libor[T](
                    currency,
                    tenor,
                    spotLag,
                    dayCounter,
                    calendar,
                    resetCurve,
                    bdConvention
                  )
                  new lib.Caplet[T](
                    libor,
                    fixingAt,
                    startAt,
                    endAt,
                    paymentAt,
                    paymentCurrency,
                    strike,
                    yieldCurve,
                    optionType
                  )
                .flatMap: caplet =>
                  market.volSurface(caplet.rate.currency, caplet.rate.tenor).flatMap: _volSurface =>
                    lib.VolatilitySurface()
                    caplet.price(market.t, volSurface)

              case other => lib.Error.Generic(s"invalid underlying $other").asLeft[Double]

      case Swaption(
            rate,
            fixingAt,
            strike,
            optionType,
            annuity,
            discountCurve
          ) => throw NotImplementedError()

      case BackwardLookingCaplet(
            startAt,
            endAt,
            rate,
            paymentCurrency,
            paymentAt,
            strike,
            optionType,
            discountCurve,
            stub,
            direction
          ) => throw NotImplementedError()
