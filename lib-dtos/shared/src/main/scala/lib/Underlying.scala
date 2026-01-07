package lib.dtos

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.Configuration
import lib.dtos.given_Codec_Period

import java.time.Period

object Underlying:
  given Configuration = Configuration.default.withDiscriminator("type")
  given Codec[Underlying] = Codec.AsObject.derivedConfigured

enum Underlying:

  case Libor(
      currency: Currency,
      tenor: Period,
      spotLag: Int,
      dayCounter: DayCounter,
      calendar: String,
      resetCurve: Curve,
      bdConvention: BusinessDayConvention
  ) extends Underlying

  case SwapRate(
      tenor: Period,
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
  ) extends Underlying

  case CompoundedSwapRate(
      tenor: Period,
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: Period,
      floatingRate: String,
      floatingPeriod: Period,
      fixedDayCounter: DayCounter,
      calendar: String,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction,
      discountCurve: Curve
  ) extends Underlying
