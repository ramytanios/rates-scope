package lib.dtos

import io.circe.Codec

case class Calendar[T](holidays: Seq[T]) derives Codec
