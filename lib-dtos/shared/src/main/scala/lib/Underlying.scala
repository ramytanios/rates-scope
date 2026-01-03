package lib.dtos

import cats.syntax.all.*
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.Configuration

import java.time.Period
import scala.util.Try

object Underlying:
  given Configuration = Configuration.default.withDiscriminator("type")
  given [T: Codec]: Codec[Underlying[T]] = Codec.AsObject.derivedConfigured
  given Codec[Period] = Codec.from(
    Decoder.decodeString.emap(str => Try(Period.parse(s"P$str")).toEither.leftMap(_.toString)),
    Encoder.encodeString.contramap[Period](_.toString)
  )

enum Underlying[T]:

  case Libor[T](
      name: String,
      currency: Currency,
      tenor: Period,
      spotLag: Int,
      dayCounter: DayCounter,
      calendar: String,
      resetCurve: Curve,
      bdConvention: BusinessDayConvention
  ) extends Underlying[T]

  case SwapRate[T](
      name: String,
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
  ) extends Underlying[T]

  case CompoundedSwapRate[T](
      name: String,
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
  ) extends Underlying[T]
