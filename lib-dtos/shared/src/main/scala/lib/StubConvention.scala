package lib.dtos

import io.circe.derivation.*

object StubConvention:
  given Configuration = Configuration.default

enum StubConvention derives ConfiguredEnumCodec:
  case Short, Long
