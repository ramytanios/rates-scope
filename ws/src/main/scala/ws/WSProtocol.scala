package ws

import io.circe.Codec
import io.circe.derivation.*
import io.circe.derivation.ConfiguredCodec

object WSProtocol:

  object Client:
    given Configuration = Configuration.default.withDiscriminator("type")
    given [T: Codec]: Codec[Client[T]] = Codec.AsObject.derivedConfigured

  enum Client[T]:
    case Price[T]() extends Client[T]
    case Arbitrage[T]() extends Client[T]
    case VolSampling[T]() extends Client[T]

  object Server:
    given Configuration = Configuration.default.withDiscriminator("type")
    given [T: Codec]: Codec[Server[T]] = Codec.AsObject.derivedConfigured

  enum Server[T]:
    case Price[T]() extends Server[T]
    case Arbitrage[T]() extends Server[T]
    case VolSampling[T]() extends Server[T]
