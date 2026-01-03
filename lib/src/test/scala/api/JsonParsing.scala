package lib.api

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

  def readResource(name: String) =
    Using.resource(Source.fromResource(name))(_.mkString)

  test("market json"):

    case class Market(
        market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
        static: dtos.Static[LocalDate]
    ) derives Codec

    val js = readResource("market.json")
    decode[Market](js).leftMap(err => lib.Error.Generic(err.getMessage)).failOrAssert(_ => ())
