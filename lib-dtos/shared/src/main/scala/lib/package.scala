package lib

import cats.syntax.all.*
import io.circe.*

import java.time.LocalDate
import scala.util.Try
import cats.Show

package object dtos:

  given Codec[LocalDate] = Codec.from(
    Decoder.decodeString.emap(str => Try(LocalDate.parse(str)).toEither.leftMap(_.toString)),
    Encoder.encodeString.contramap(_.toString)
  )

  abstract class OT[V: Encoder: Decoder]:
    opaque type Type = V
    inline def apply(v: V): Type = v
    extension (t: Type) inline def value: V = t
    given Codec[Type] = Codec.from(summon[Decoder[V]], summon[Encoder[V]])

  abstract class OTAsKey[V: Encoder: Decoder: Show](fromKey: String => Option[V]) extends OT[V]:
    given KeyEncoder[Type] = KeyEncoder.instance[Type](t => Show[V].show(t.value))
    given KeyDecoder[Type] = KeyDecoder.instance[Type](s => fromKey(s).map(apply))

  type RateId = RateId.Type
  object RateId extends OTAsKey[String](Option.apply)

  type CalendarId = CalendarId.Type
  object CalendarId extends OTAsKey[String](Option.apply)

  type CurveId = CurveId.Type
  object CurveId extends OTAsKey[String](Option.apply)

  type Moneyness = Moneyness.Type
  object Moneyness extends OT[Double]
