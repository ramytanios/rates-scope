package lib.dtos

import io.circe.derivation.*

object BusinessDayConvention:
  given Configuration = Configuration.default

enum BusinessDayConvention derives ConfiguredEnumCodec:
  case Following, Preceding, ModifiedFollowing
