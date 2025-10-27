package lib

import lib.*
import lib.quantities.*

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

  def price(t: T, volSurface: VolatilitySurface[T]): Either[Error, Double] =
    val f = rate.forward(fixingAt)
    val vol = volSurface(fixingAt)(strike)
