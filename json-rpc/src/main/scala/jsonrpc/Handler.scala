package jsonrpc

import io.circe.*
import io.circe.syntax.*
import lib.api.Api
import cats.syntax.all.*
import lib.api.Market
import lib.dtos
import lib.dtos.given_Codec_LocalDate
import lib.dtos.given_Codec_Period

import java.time.LocalDate
import java.time.Period

object Handler:

  case class PriceParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      payoff: dtos.Payoff[LocalDate],
      static: dtos.Static[LocalDate]
  ) derives Codec

  case class ArbitrageParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      static: dtos.Static[LocalDate],
      currency: dtos.Currency,
      tenor: Period,
      expiry: Period
  ) derives Codec

  case class ArbitrageMatrixParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      static: dtos.Static[LocalDate],
      currency: dtos.Currency
  ) derives Codec

  case class VolSamplingParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      static: dtos.Static[LocalDate],
      currency: dtos.Currency,
      tenor: Period,
      expiry: Period,
      nSamplesMiddle: Int,
      nSamplesTail: Int,
      nStdvsTail: Int
  ) derives Codec

  private def impl[P: Decoder](
      r: JsonRpc.Request,
      f: P => Either[lib.Error, Json]
  ): JsonRpc.Response =
    r.params.fold(
      JsonRpc.error(JsonRpc.ErrorCode.InvalidParams, "missing params")
    )(_.as[P].fold(
      th => JsonRpc.error(JsonRpc.ErrorCode.InvalidParams, th.getMessage),
      f(_).fold(
        th => JsonRpc.error(JsonRpc.ErrorCode.InternalError, th.getMessage),
        js => JsonRpc.success(js, r.id)
      )
    ))

  def apply(request: JsonRpc.Request): JsonRpc.Response =
    request.method match
      case "price" => impl[PriceParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            new Api(market).price(params.payoff).map(res => JsonObject("price" -> res.asJson).toJson)
        )

      case "arbitrage" => impl[ArbitrageParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            new Api(market).arbitrageCheck(params.currency, params.tenor, params.expiry)
              .map(res =>
                JsonObject("arbitrage" -> res.map(arb =>
                  arb match
                    case lib.Arbitrage.LeftAsymptotic =>
                      JsonObject("type" -> "LeftAsymptotic".asJson).toJson
                    case lib.Arbitrage.RightAsymptotic =>
                      JsonObject("type" -> "RightAsymptotic".asJson).toJson
                    case lib.Arbitrage.Density(leftStrike, rightStrike) =>
                      JsonObject(
                        "type" -> "Density".asJson,
                        "between" -> List(leftStrike, rightStrike).asJson
                      ).toJson
                ).asJson).toJson
              )
        )

      case "arbitragematrix" => impl[ArbitrageMatrixParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            market.volCube(params.currency).flatMap: volCube =>
              val tenors = volCube.cube.keysIterator.toList
              val expiries = volCube.cube.values.flatMap(_.surface.keysIterator).toList.distinct
              val api = new Api(market)
              tenors.flatTraverse: tenor =>
                expiries.traverse: expiry =>
                  api.arbitrageCheck(params.currency, tenor, expiry).map((tenor, expiry, _))
              .map: matrix =>
                JsonObject("matrix" -> matrix.map((te, ex, ar) =>
                  val arbJson = ar.map:
                    case lib.Arbitrage.LeftAsymptotic =>
                      JsonObject("type" -> "LeftAsymptotic".asJson).toJson
                    case lib.Arbitrage.RightAsymptotic =>
                      JsonObject("type" -> "RightAsymptotic".asJson).toJson
                    case lib.Arbitrage.Density(leftStrike, rightStrike) =>
                      JsonObject(
                        "type" -> "Density".asJson,
                        "between" -> List(leftStrike, rightStrike).asJson
                      ).toJson
                  (te, ex, arbJson).asJson
                ).asJson).toJson
        )

      case "volsampling" => impl[VolSamplingParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            new Api(market).sampleVolSkew(
              params.currency,
              params.tenor,
              params.expiry,
              params.nSamplesMiddle,
              params.nSamplesTail,
              params.nStdvsTail
            ).map(res =>
              JsonObject(
                "quotedStrikes" -> res.quotedStrikes.asJson,
                "quotedVols" -> res.quotedVols.asJson,
                "quotedPdf" -> res.quotedPdf.asJson,
                "strikes" -> res.strikes.asJson,
                "vols" -> res.vols.asJson,
                "pdf" -> res.pdf.asJson,
                "fwd" -> res.fwd.asJson
              ).toJson
            )
        )

      case other => JsonRpc.error(JsonRpc.ErrorCode.MethodNotFound, s"method $other not found")
