package lib.dtos

import io.circe.derivation.*

object Direction:
  given Configuration = Configuration.default

enum Direction derives ConfiguredEnumCodec:
  case Backward, Forward
