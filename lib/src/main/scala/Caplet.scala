package lib

import cats.syntax.all.*
import lib.quantities.*
import lib.syntax.*

class Caplet[T: DateLike](
    val rate: Libor[T],
    val fixingAt: T,
    val startAt: T,
    val endAt: T,
    val paymentAt: T,
    val paymentCurrency: Currency,
    val strike: Double,
    val discountCurve: YieldCurve[T],
    val optionType: OptionType
):

  def price(ref: T, volSurface: VolatilitySurface[T]): Either[Error, Double] =

    val (_, interestEndAt) = rate.interestPeriod(fixingAt)

    Either.raiseWhen(
      rate.currency != paymentCurrency
    )(Error.Generic("vanilla price does not support quanto"))
      .flatMap: _ =>
        Either.raiseWhen((interestEndAt - paymentAt).abs > 7)(
          Error.Generic(s"vanilla pricer does not allow payment convexity")
        )
      .map: _ =>
        val f = rate.forward(fixingAt)
        val d = discountCurve.discount(paymentAt)
        val dt = ref.yearFractionTo(fixingAt)(using DateLike[T], DayCounter.Act365)
        val vol = volSurface(fixingAt)(strike)
        val dcf = startAt.yearFractionTo(endAt)(using DateLike[T], rate.dayCounter)
        dcf * bachelier.price(optionType, f, strike, dt.toDouble, vol, d)
