package lib.dtos

import io.circe.derivation.*

object DayCounter:
  given Configuration = Configuration.default

enum DayCounter derives ConfiguredEnumCodec:
  case Act365, Act360
