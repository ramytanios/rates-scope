package entry

import lib.DateLike
import lib.dtos.*
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class VanillaPricerSuite extends munit.FunSuite with EitherSyntax:

  test("caplet price"):

    val t = d"2025-10-12"

    val rate: dtos.Underlying[LocalDate] = dtos.Underlying.Libor(
      "libor-rate",
      Currency.USD,
      Tenor.`3M`.toPeriod,
      2,
      dtos.DayCounter.Act360,
      dtos.Calendar[LocalDate](Nil),
      dtos.Curve(Currency.USD, "single-curve"),
      BusinessDayConvention.ModifiedFollowing
    )

    val market = Market(
      ref = t,
      rates = Map("libor-rate" -> rate),
      curves =
        Map(
          dtos.Curve(Currency.USD, "single-curve") ->
            dtos.YieldCurve.ContinuousCompounding(0.02)
        ),
      fixingsByRate = Map.empty,
      volConventions = Map(Currency.USD -> Map(Tenor.`3M` -> rate)),
      volatilities = Map(
        Currency.USD -> dtos.VolatilityCube(
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
                  ).map((m, v) => m -> v / 10000)
                )
              )
            )
          )
        )
      )
    )

    val fixingAt = d"2026-10-12"
    val startAt = d"2026-10-14"
    val endAt = d"2027-01-14"

    val caplet = dtos.Payoff.Caplet(
      "libor-rate",
      fixingAt,
      startAt,
      endAt,
      endAt,
      Currency.USD,
      0.009887915724457295,
      dtos.Curve(Currency.USD, "single-curve"),
      OptionType.Call
    )

    val pricer = new VanillaPricer(market)

    pricer.price(caplet).failOrAssert: price =>
      assertEqualsDouble(price, 0.0025670413971979954, 1e-10)
