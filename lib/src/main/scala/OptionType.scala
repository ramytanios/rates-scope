package lib

enum OptionType(val sign: Int):
  case Call extends OptionType(1)
  case Put extends OptionType(-1)

object OptionType:

  extension (ot: OptionType)
    def intrinsic(value: Double, strike: Double): Double =
      math.max(ot.sign * (value - strike), 0.0)
