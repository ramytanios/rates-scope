package lib

import cats.syntax.all.*
import io.circe.*

import java.time.LocalDate
import java.time.Period
import scala.util.Try

package object dtos:

  given Codec[LocalDate] = Codec.from(
    Decoder.decodeString.emap(str => Try(LocalDate.parse(str)).toEither.leftMap(_.toString)),
    Encoder.encodeString.contramap(_.toString)
  )

  given KeyDecoder[Period] = KeyDecoder.instance(str => Try(Period.parse(s"P$str")).toOption)

  given KeyEncoder[Period] = KeyEncoder.encodeKeyString.contramap[Period](_.toString)
