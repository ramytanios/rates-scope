package lib.dtos

import io.circe.Codec

case class CcyMarket[T](
    rates: Map[String, Underlying[T]],
    curves: Map[String, YieldCurve[T]],
    fixings: Map[String, Seq[Fixing[T]]],
    volatility: VolatilityCube,
    volConventions: VolatilityMarketConventions[T]
) derives Codec
