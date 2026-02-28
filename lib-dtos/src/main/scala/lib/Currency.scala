package lib.dtos

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.derivation.*

import scala.util.Try

object Currency:
  given Configuration = Configuration.default
  given KeyEncoder[Currency] = KeyEncoder.encodeKeyString.contramap[Currency](_.toString)
  given KeyDecoder[Currency] = KeyDecoder.instance(str => Try(Currency.valueOf(str)).toOption)

enum Currency derives ConfiguredEnumCodec:
  case AUD
  case BRL
  case CHF
  case CNY
  case USD
  case EUR
  case GBP
  case HUF
  case ILS
  case INR
  case JPY
  case MXN
  case NOK
  case NZD
  case PLN
  case SEK
  case SGD
  case TRY
  case TWD
