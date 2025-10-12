package dtos

import io.circe.Codec
import io.circe.generic.semiauto.*

import java.time as jt
import java.util as ju

object WSProtocol:

  enum Client:
    case Ping

  object Client:
    given Codec[Client] = deriveCodec[Client]

  enum Server:
    case Pong

  object Server:
    given Codec[Server] = deriveCodec[Server]
