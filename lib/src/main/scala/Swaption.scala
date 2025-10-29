package lib

import cats.syntax.all.*
import lib.*
import lib.quantities.*
import lib.syntax.*

enum Annuity:
  case Physical, Cash

class Swaption[T: DateLike](
    val rate: SwapLike[T],
    val fixingAt: T,
    val strike: Double,
    val optionType: OptionType,
    val annuity: Annuity,
    val discountCurve: YieldCurve[T]
):

  given DayCounter = rate.fixedDayCounter

  def price(t: T, volSurface: VolatilitySurface[T]): Either[Error, Double] =
    val fwd = rate.forward(fixingAt)
    val vol = volSurface(fixingAt)(strike)
    val (swapStartAt, swapEndAt) = rate.interestPeriod(fixingAt)
    val fixed = rate.fixedSchedule(swapStartAt, swapEndAt)
    val discount = annuity match
      case Annuity.Physical => // enter into a real swap contract
        val a = fixed.foldLeft(0.0):
          case (acc, FixedCoupon(startAt, endAt, paymentAt)) =>
            acc + rate.discountCurve.discount(paymentAt) * startAt.yearFractionTo(endAt).toDouble
        val adj = discountCurve.discount(fixingAt) / rate.discountCurve.discount(fixingAt)
        a * adj
      case Annuity.Cash => // replace physical annuity with an approx. cash payment at swap start date
        var cash = 0.0
        for i <- fixed.indices.reverse do
          val dcf = fixed(i).from.yearFractionTo(fixed(i).to).toDouble
          val discount = 1.0 / (1.0 + fwd * dcf)
          cash = (dcf + cash) * discount
        cash * discountCurve.discount(swapStartAt)
    val dt = t.yearFractionTo(fixingAt)(using DateLike[T], DayCounter.Act365).toDouble
    bachelier.price(optionType, fwd, strike, dt, vol, discount).asRight[Error]
