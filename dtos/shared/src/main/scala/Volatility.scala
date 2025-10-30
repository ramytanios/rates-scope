package dtos

import java.time.Period
import lib.dtos.*

case class VolatiltySkew(skew: Seq[(Double, Double)])

case class VolatilitySurface[T](surface: Map[Period, VolatiltySkew])

case class VolatilityCube[T](cube: Map[Period, VolatilitySurface[T]])

case class VolatilityMarketConventions[T](rates: Map[Currency, Map[Period, Underlying[T]]])
