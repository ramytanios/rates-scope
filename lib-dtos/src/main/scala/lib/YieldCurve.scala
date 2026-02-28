package lib.dtos

import io.circe.Codec
import io.circe.derivation.*

object YieldCurve:
  given Configuration = Configuration.default.withDiscriminator("type")
  given [T: Codec]: Codec[YieldCurve[T]] = Codec.AsObject.derivedConfigured

enum YieldCurve[T]:
  case Discounts[T](discounts: Seq[(T, Double)]) extends YieldCurve[T]
  case ContinuousCompounding[T](rate: Double) extends YieldCurve[T]
