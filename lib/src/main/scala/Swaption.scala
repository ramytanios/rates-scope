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
    val f = rate.forward(fixingAt)
    val vol = volSurface(fixingAt)(strike)
    val (from, to) = rate.interestPeriod(fixingAt)
    val fixed = rate.fixedSchedule(from, to)
    val d = annuity match
      case Annuity.Physical =>
        val a = fixed.foldLeft(0.0):
          case (acc, FixedCoupon(startAt, endAt, paymentAt)) =>
            acc + rate.discountCurve.discount(paymentAt) * startAt.yearFractionTo(endAt).toDouble
        val adj = discountCurve.discount(fixingAt) / rate.discountCurve.discount(fixingAt)
        a * adj
      case Annuity.Cash => throw NotImplementedError()
    val dt = t.yearFractionTo(fixingAt)(using DateLike[T], DayCounter.Act365).toDouble
    bachelier.price(optionType, f, strike, dt, vol, d).asRight[Error]
