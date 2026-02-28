package lib.dtos

import io.circe.Codec

case class Fixing[T](t: T, value: Double) derives Codec
