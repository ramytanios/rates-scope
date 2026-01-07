package lib.dtos

import io.circe.Codec

case class CcyMarket[T](
    rates: Map[String, Underlying],
    curves: Map[String, YieldCurve[T]],
    fixings: Map[String, Seq[Fixing[T]]],
    volatility: VolatilityCube,
    volConventions: VolatilityMarketConventions
) derives Codec
