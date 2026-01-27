package lib.dtos

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.Configuration

object Underlying:
  given Configuration = Configuration.default.withDiscriminator("type")
  given Codec[Underlying] = Codec.AsObject.derivedConfigured

enum Underlying:

  case Libor(
      currency: Currency,
      tenor: Tenor,
      spotLag: Int,
      dayCounter: DayCounter,
      calendar: String,
      resetCurve: Curve,
      bdConvention: BusinessDayConvention
  ) extends Underlying

  case SwapRate(
      tenor: Tenor,
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: Tenor,
      floatingRate: String,
      fixedDayCounter: DayCounter,
      calendar: String,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction,
      discountCurve: Curve
  ) extends Underlying

  case CompoundedSwapRate(
      tenor: Tenor,
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: Tenor,
      floatingRate: String,
      floatingPeriod: Tenor,
      fixedDayCounter: DayCounter,
      calendar: String,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction,
      discountCurve: Curve
  ) extends Underlying
