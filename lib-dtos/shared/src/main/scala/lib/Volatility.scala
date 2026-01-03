package lib.dtos

import io.circe.*
import io.circe.derivation.*

import java.time.Period

case class VolatiltySkew(skew: Seq[(Double, Double)]) derives Codec

case class VolatilitySurface(surface: Map[Period, VolatiltySkew]) derives Codec

case class VolatilityCube(cube: Map[Period, VolatilitySurface]) derives Codec

case class VolatilityMarketConventions[T](rates: Map[Period, Underlying[T]]) derives Codec
