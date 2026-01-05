package jsonrpc

import cats.effect.IO

object Handler:

  private val jsonrpc = "2.0"

  def apply(request: JsonRpc.Request): IO[JsonRpc.Response] =
    request.method match
      case "price"       => ???
      case "arbitrage"   => ???
      case "volsampling" => ???
      case other =>
        IO.pure(
          JsonRpc.Response(
            jsonrpc,
            None,
            Some(
              JsonRpc.Error(
                JsonRpc.ErrorCode.MethodNotFound.code,
                s"Method $other not found",
                None
              )
            ),
            request.id
          )
        )
