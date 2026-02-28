package lib.dtos

import io.circe.Codec

case class CcyMarket[T](
    rates: Map[RateId, Underlying],
    curves: Map[CurveId, YieldCurve[T]],
    fixings: Map[RateId, Seq[Fixing[T]]],
    volatility: VolatilityCube,
    volConventions: VolatilityMarketConventions
) derives Codec
