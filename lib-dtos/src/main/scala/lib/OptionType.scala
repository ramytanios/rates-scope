package lib.dtos

import io.circe.derivation.*

object OptionType:
  given Configuration = Configuration.default

enum OptionType(val sign: Int) derives ConfiguredEnumCodec:
  case Call extends OptionType(1)
  case Put extends OptionType(-1)
