package lib.dtos

import io.circe.derivation.*

object Annuity:
  given Configuration = Configuration.default

enum Annuity derives ConfiguredEnumCodec:
  case Physical, Cash
