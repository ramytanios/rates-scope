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
    val discountWith: Curve,
    val optionType: OptionType
):

  def price(using Market[T]): Either[Error, Double] =

    val ref = summon[Market[T]].ref

    val (_, interestEndAt) = rate.interestPeriod(fixingAt)

    Either
      .raiseWhen(
        discountWith.ccy != paymentCurrency
      )(Error.Generic("discount curve and payment currencies mismatch"))
      .flatMap: _ =>
        Either.raiseWhen(
          rate.currency != paymentCurrency
        )(Error.Generic("vanilla price does not support quanto"))
      .flatMap: _ => // TODO: should we count business days ?
        Either.raiseWhen(rate.calendar.countBusinessDays(interestEndAt, paymentAt).abs > 7)(
          Error.Generic(s"vanilla pricer does not allow payment convexity")
        )
      .flatMap: _ =>
        rate.forward.flatMap: forwardCurve =>
          summon[Market[T]].yieldCurve(discountWith).flatMap: yieldCurve =>
            val f = forwardCurve(fixingAt)
            val d = yieldCurve.discount(paymentAt)
            val dt = ref.yearFractionTo(fixingAt)(using DateLike[T], DayCounter.Act365)
            summon[Market[T]].volSurface(rate.currency, rate.tenor).map: volSurface =>
              val vol = volSurface(fixingAt)(strike)
              val dcf = startAt.yearFractionTo(endAt)(using DateLike[T], rate.dayCounter)
              dcf * bachelier.price(optionType, f, strike, dt.toDouble, vol, d)
