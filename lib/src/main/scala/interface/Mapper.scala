package lib.interface

import cats.syntax.all.*
import lib.dtos
import lib.quantities.*

class Mapper[T: lib.DateLike](market: Market[T]):

  def buildYieldCurve(curve: dtos.Curve): Either[lib.Error, lib.YieldCurve[T]] =
    market.yieldCurve(curve).map:
      case dtos.YieldCurve.Discounts(discounts) =>
        lib.YieldCurve(market.t, discounts.toIndexedSeq)
      case dtos.YieldCurve.ContinuousCompounding(rate) =>
        lib.YieldCurve.continuousCompounding(market.t, rate)

  def buildCalendar(calendar: dtos.Calendar[T]): Either[lib.Error, lib.Calendar[T]] =
    Either.catchNonFatal(lib.Calendar.fromHolidays(calendar.holidays.toIndexedSeq)).leftMap: th =>
      lib.Error.Generic(s"unable to build calendar: ${th.getMessage}")

  def buildFixings(rate: String): Either[lib.Error, Map[T, Double]] =
    market.fixings(rate).orElse(Seq.empty.asRight).flatMap: fixings =>
      fixings.groupBy(_.t).toSeq.traverse: (fixingAt, all) =>
        all.headOption.toRight(MarketError.FixingAt(rate, fixingAt)).map(_.value).tupleLeft(fixingAt)
      .map(_.toMap)

  def buildVolSurface(
      currency: dtos.Currency,
      tenor: lib.quantities.Tenor
  ): Either[lib.Error, lib.VolatilitySurface[T]] =
    market.volSurface(currency, tenor).flatMap: surface =>
      buildMarketRate(currency, tenor).map: rate =>
        val skews = surface.surface.toList.map:
          case (expTenor, dtos.VolatiltySkew(skew)) =>
            val (moneynesses, vols) = skew.unzip
            val expiry = rate.calendar.addBusinessPeriod(market.t, expTenor)(using rate.bdConvention)
            val forward = rate.forward(expiry)
            val strikes = moneynesses.map(forward + _)
            expiry -> lib.Lazy(lib.VolatilitySkew(strikes.toIndexedSeq, vols.toIndexedSeq))
        lib.VolatilitySurface(market.t, rate.forward, skews.toIndexedSeq)

  def buildVolCube(currency: dtos.Currency): Either[lib.Error, lib.VolatilityCube[T]] =
    market.volCube(currency).flatMap: volCube =>
      val surfaces = volCube.cube.toList.traverse: (tenor, _) =>
        buildVolSurface(currency, tenor).tupleLeft(Tenor(tenor))
      .map(_.toIndexedSeq)
      val forwards = volCube.cube.keys.toList.traverse: tenor =>
        buildMarketRate(currency, tenor).map(_.forward).tupleLeft(Tenor(tenor))
      .map(_.toMap)
      (surfaces, forwards).tupled.map: (surfaces, forwards) =>
        lib.VolatilityCube[T](surfaces, forwards)

  def buildMarketRate(currency: dtos.Currency, tenor: Tenor): Either[lib.Error, lib.Underlying[T]] =
    market.volatilityConventions(currency, tenor).flatMap:
      case libor: dtos.Underlying.Libor[T]             => buildLibor(libor.name)
      case swap: dtos.Underlying.SwapRate[T]           => buildSwapRate(swap.name)
      case swap: dtos.Underlying.CompoundedSwapRate[T] => buildCompoundedSwapRate(swap.name)

  def buildLibor(rate: String): Either[lib.Error, lib.Libor[T]] =
    market.rate(rate).flatMap:
      case libor: dtos.Underlying.Libor[T] =>
        buildYieldCurve(libor.resetCurve).flatMap: resetCurve =>
          buildCalendar(libor.calendar).map: calendar =>
            new lib.Libor[T](
              libor.currency,
              Tenor(libor.tenor),
              libor.spotLag,
              toDayCounter(libor.dayCounter),
              calendar,
              resetCurve,
              libor.bdConvention
            )
      case _ => lib.Error.Generic(s"$rate is not a libor").asLeft

  def buildSwapRate(rate: String): Either[lib.Error, lib.SwapRate[T]] =
    market.rate(rate).flatMap:
      case swapRate: dtos.Underlying.SwapRate[T] =>
        buildYieldCurve(swapRate.discountCurve).flatMap: discountCurve =>
          buildLibor(swapRate.floatingRate).flatMap: liborRate =>
            buildCalendar(swapRate.calendar).map: calendar =>
              new lib.SwapRate[T](
                swapRate.tenor,
                swapRate.spotLag,
                swapRate.paymentDelay,
                Tenor(swapRate.fixedPeriod),
                liborRate,
                toDayCounter(swapRate.fixedDayCounter),
                calendar,
                swapRate.bdConvention,
                swapRate.stub,
                swapRate.direction,
                discountCurve
              )
      case _ => lib.Error.Generic(s"$rate is not a swap rate").asLeft

  def buildCompoundedSwapRate(rate: String): Either[lib.Error, lib.CompoundedSwapRate[T]] =
    market.rate(rate).flatMap:
      case libor: dtos.Underlying.CompoundedSwapRate[T] =>
        buildYieldCurve(libor.discountCurve).flatMap: discountCurve =>
          buildLibor(libor.floatingRate).flatMap: liborRate =>
            buildCalendar(libor.calendar).map: calendar =>
              new lib.CompoundedSwapRate[T](
                libor.tenor,
                libor.spotLag,
                libor.paymentDelay,
                Tenor(libor.fixedPeriod),
                liborRate,
                Tenor(libor.fixedPeriod),
                toDayCounter(libor.fixedDayCounter),
                calendar,
                libor.bdConvention,
                libor.stub,
                libor.direction,
                discountCurve
              )
      case _ => lib.Error.Generic(s"$rate is not a compounded swap rate").asLeft

  def buildCaplet(caplet: dtos.Payoff.Caplet[T]): Either[lib.Error, lib.Caplet[T]] =
    buildLibor(caplet.rate).flatMap: libor =>
      buildYieldCurve(caplet.discountCurve).map: discountCurve =>
        new lib.Caplet[T](
          libor,
          caplet.fixingAt,
          caplet.startAt,
          caplet.endAt,
          caplet.paymentAt,
          caplet.paymentCurrency,
          caplet.strike,
          discountCurve,
          caplet.optionType,
          lib.Detachment.default[T]
        )

  def buildSwaption(swaption: dtos.Payoff.Swaption[T]): Either[lib.Error, lib.Swaption[T]] =
    buildSwapRate(swaption.rate)
      .orElse(buildCompoundedSwapRate(swaption.rate))
      .flatMap: rate =>
        buildYieldCurve(swaption.discountCurve).map: discountCurve =>
          new lib.Swaption[T](
            rate,
            swaption.fixingAt,
            swaption.strike,
            swaption.optionType,
            swaption.annuity,
            discountCurve,
            lib.Detachment.default[T]
          )

  def buildBackwardLookingCaplet(caplet: dtos.Payoff.BackwardLookingCaplet[T])
      : Either[lib.Error, lib.BackwardLookingCaplet[T]] =
    buildLibor(caplet.rate).flatMap: libor =>
      buildYieldCurve(caplet.discountCurve).map: discountCurve =>
        new lib.BackwardLookingCaplet[T](
          caplet.startAt,
          caplet.endAt,
          libor,
          caplet.paymentCurrency,
          caplet.paymentAt,
          caplet.strike,
          caplet.optionType,
          discountCurve,
          caplet.stub,
          caplet.direction,
          lib.Detachment.default[T]
        )
