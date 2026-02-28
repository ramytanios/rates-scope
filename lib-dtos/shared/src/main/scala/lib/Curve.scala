package lib.dtos

import io.circe.Codec

case class Curve(currency: Currency, name: CurveId) derives Codec
