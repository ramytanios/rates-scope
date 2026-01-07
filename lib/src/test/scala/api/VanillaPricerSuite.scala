package lib.api

import lib.DateLike
import lib.EitherSyntax
import lib.api.*
import lib.dtos
import lib.dtos.VolUnit
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class VanillaPricerSuite extends munit.FunSuite with EitherSyntax:

  test("caplet price"):

    val t = d"2025-10-12"

    val rate: dtos.Underlying = dtos.Underlying.Libor(
      dtos.Currency.USD,
      Tenor.`3M`.toPeriod,
      2,
      dtos.DayCounter.Act360,
      "NO_HOLIDAYS",
      dtos.Curve(dtos.Currency.USD, "SINGLE_CURVE"),
      dtos.BusinessDayConvention.ModifiedFollowing
    )

    val volConventions = dtos.VolatilityMarketConventions(
      Tenor.`10Y`.toPeriod,
      dtos.VolatilityMarketConventions.Libor(
        dtos.Currency.USD,
        2,
        dtos.DayCounter.Act360,
        "NO_HOLIDAYS",
        dtos.Curve(dtos.Currency.USD, "SINGLE_CURVE"),
        dtos.BusinessDayConvention.ModifiedFollowing
      ),
      dtos.VolatilityMarketConventions.SwapRate(
        2,
        0,
        Tenor.`3M`.toPeriod,
        "LIBOR_RATE",
        dtos.DayCounter.Act360,
        "NO_HOLIDAYS",
        dtos.BusinessDayConvention.ModifiedFollowing,
        dtos.StubConvention.Short,
        dtos.Direction.Backward,
        dtos.Curve(dtos.Currency.USD, "SINGLE_CURVE")
      )
    )

    val market = Market(
      tRef = t,
      rates = Map("LIBOR_RATE" -> rate),
      curves =
        Map(
          dtos.Curve(dtos.Currency.USD, "SINGLE_CURVE") ->
            dtos.YieldCurve.ContinuousCompounding(0.02)
        ),
      fixingsByRate = Map.empty,
      volConventions = Map(dtos.Currency.USD -> volConventions),
      volatilities = Map(
        dtos.Currency.USD -> dtos.VolatilityCube(
          Map(
            Tenor.`3M`.toPeriod -> dtos.VolatilitySurface(
              Map(
                Tenor.`1Y`.toPeriod -> dtos.VolatiltySkew(
                  Seq(
                    -0.0200 -> 100.0,
                    -0.0100 -> 80.0,
                    -0.0050 -> 72.0,
                    -0.0025 -> 70.0,
                    +0.0000 -> 69.0,
                    +0.0025 -> 71.0,
                    +0.0050 -> 74.0,
                    +0.0100 -> 90.0,
                    +0.0200 -> 93.0
                  )
                )
              )
            )
          ),
          VolUnit.BpPerYear
        )
      ),
      calendars = Map("NO_HOLIDAYS" -> dtos.Calendar[LocalDate](Nil))
    )

    val fixingAt = d"2026-10-12"
    val startAt = d"2026-10-14"
    val endAt = d"2027-01-14"

    val caplet = dtos.Payoff.Caplet(
      "LIBOR_RATE",
      fixingAt,
      startAt,
      endAt,
      endAt,
      dtos.Currency.USD,
      0.009887915724457295,
      dtos.Curve(dtos.Currency.USD, "SINGLE_CURVE"),
      dtos.OptionType.Call
    )

    new Api(market).price(caplet).failOrAssert: price =>
      assertEqualsDouble(price, 0.0025670027485647476, 1e-10)
