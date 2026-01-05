package jsonrpc

import io.circe.Codec
import io.circe.Json
import io.circe.derivation.*

object JsonRpc:

  // https://www.jsonrpc.org/specification

  private val jsonrpc = "2.0"

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
      id: Option[String]
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

  def error(code: ErrorCode, message: String): Response =
    Response(jsonrpc, None, Some(Error(code.code, message, None)), None)

  def success(result: Json, id: String): Response =
    Response(jsonrpc, Some(result), None, Some(id))
