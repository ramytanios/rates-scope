package lib.dtos

import io.circe.*
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.*
import io.circe.derivation.Configuration
import lib.dtos.given_Codec_Period

import java.time.Period

object VolUnit:
  given Configuration = Configuration.default

enum VolUnit derives ConfiguredEnumCodec:
  case BpPerYear

case class VolatiltySkew(skew: Seq[(Double, Double)]) derives Codec

case class VolatilitySurface(surface: Map[Period, VolatiltySkew]) derives Codec

case class VolatilityCube(cube: Map[Period, VolatilitySurface], unit: VolUnit) derives Codec

case class VolatilityMarketConventions(
    boundaryTenor: Period,
    liborRate: VolatilityMarketConventions.Libor,
    swapRate: VolatilityMarketConventions.SwapRate
) derives Codec

object VolatilityMarketConventions:

  case class Libor(
      currency: Currency,
      spotLag: Int,
      dayCounter: DayCounter,
      calendar: String,
      resetCurve: Curve,
      bdConvention: BusinessDayConvention
  ) derives Codec

  case class SwapRate(
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: Period,
      floatingRate: String,
      fixedDayCounter: DayCounter,
      calendar: String,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction,
      discountCurve: Curve
  ) derives Codec
