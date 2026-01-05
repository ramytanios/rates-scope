package jsonrpc

import io.circe.derivation.*
import io.circe.Json
import io.circe.Codec

object JsonRpc:

  // https://www.jsonrpc.org/specification

  case class Request(
      jsonrpc: String,
      method: String,
      params: Option[Json],
      id: String
  ) derives Codec

  case class Response(
      jsonrpc: String,
      result: Option[Json],
      error: Option[Error],
      id: String
  ) derives Codec

  case class Error(
      code: Int,
      message: String,
      data: Option[Json]
  ) derives Codec

  enum ErrorCode(val code: Int):
    case ParseError extends ErrorCode(-32700)
    case InvalidRequest extends ErrorCode(-32600)
    case MethodNotFound extends ErrorCode(-32601)
    case InvalidParams extends ErrorCode(-32602)
    case InternalError extends ErrorCode(-32603)
    // TODO add more
