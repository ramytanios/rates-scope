package lib.dtos

import io.circe.Codec

case class Static[T](calendars: Map[CalendarId, Calendar[T]]) derives Codec
