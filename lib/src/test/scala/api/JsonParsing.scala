package lib.api

import lib.literals.*
import cats.syntax.all.*
import io.circe.Codec
import io.circe.parser.*
import lib.EitherSyntax
import lib.dtos
import lib.dtos.given_Codec_LocalDate

import java.time.LocalDate
import scala.io.Source
import scala.util.Using

class JsonParsing extends munit.FunSuite with EitherSyntax:

  private case class Js(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      static: dtos.Static[LocalDate]
  ) derives Codec

  def readResource(name: String) =
    Using.resource(Source.fromResource(name))(_.mkString)

  test("parsing market json"):

    val js = readResource("market.json")
    decode[Js](js).leftMap(err => lib.Error.Generic(err.getMessage)).failOrAssert(_ => ())

  test("caplet price"):

    val js = readResource("market.json")

    decode[Js](js)
      .leftMap: err =>
        lib.Error.Generic(err.getMessage)
      .flatMap: js =>
        val market = Market.default(js.tRef, js.market, js.static)
        val caplet = dtos.Payoff.Caplet(
          "LIBOR_RATE",
          d"2026-10-12",
          d"2026-10-14",
          d"2027-01-14",
          d"2027-01-14",
          dtos.Currency.USD,
          0.009887915724457295,
          dtos.Curve(dtos.Currency.USD, "SINGLE_CURVE"),
          dtos.OptionType.Call
        )
        new Api(market).price(caplet)
      .failOrAssert: price =>
        assertEqualsDouble(price, 0.0025670027485647476, 1e-10)
