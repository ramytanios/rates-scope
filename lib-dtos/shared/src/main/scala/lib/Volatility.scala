package lib.dtos

import java.time.Period

case class VolatiltySkew(skew: Seq[(Double, Double)])

case class VolatilitySurface(surface: Map[Period, VolatiltySkew])

case class VolatilityCube(cube: Map[Period, VolatilitySurface])

case class VolatilityMarketConventions[T](rates: Map[Currency, Map[Period, Underlying[T]]])
