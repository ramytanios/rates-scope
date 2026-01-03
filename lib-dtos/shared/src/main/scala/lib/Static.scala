package lib.dtos

import io.circe.Codec

case class Static[T](calendars: Map[String, Calendar[T]]) derives Codec
