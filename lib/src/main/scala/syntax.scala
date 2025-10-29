package lib

import lib.dtos.*

object syntax extends DateLike.Syntax with CollectionSyntax:

  extension (ot: OptionType)
    def intrinsic(value: Double, strike: Double): Double =
      math.max(ot.sign * (value - strike), 0.0)
