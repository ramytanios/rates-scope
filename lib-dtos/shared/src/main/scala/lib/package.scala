package lib

import cats.syntax.all.*
import io.circe.*

import java.time.LocalDate
import scala.util.Try

package object dtos:

  given Codec[LocalDate] = Codec.from(
    Decoder.decodeString.emap(str => Try(LocalDate.parse(str)).toEither.leftMap(_.toString)),
    Encoder.encodeString.contramap(_.toString)
  )
