package entry

import cats.syntax.all.*
import lib.dtos.*
import lib.quantities.*

// TODO work on nomenclature of variables
class Builder[T: lib.DateLike](market: Market[T]):

  def buildYieldCurve(curve: dtos.Curve): Either[MarketError, lib.YieldCurve[T]] =
    market.yieldCurve(curve).map:
      case dtos.YieldCurve.Discounts(discounts) =>
        lib.YieldCurve(market.t, discounts.toIndexedSeq)
      case dtos.YieldCurve.ContinuousCompounding(rate) =>
        lib.YieldCurve.continuousCompounding(market.t, rate)

  def buildCalendar(cal: dtos.Calendar[T]): lib.Calendar[T] =
    lib.Calendar.fromHolidays(cal.holidays)

  def buildFixings(rate: String): Either[MarketError, Map[T, Double]] =
    market.fixings(rate).flatMap: fixings =>
      fixings.groupBy(_.t).toSeq.traverse: (t, fs) =>
        fs.headOption.toRight(MarketError.FixingAt(rate, t)).map(_.value).tupleLeft(t)
      .map(_.toMap)

  def buildVolSurface(
      currency: Currency,
      tenor: lib.quantities.Tenor
  ): Either[lib.Error, lib.VolatilitySurface[T]] =
    market.volSurface(currency, tenor).flatMap: surf =>
      surf.surface.toList.traverse:
        case (expTenor, dtos.VolatiltySkew(skew)) =>
          val (ms, vs) = skew.unzip
          buildMarketRate(currency, tenor).map: rate =>
            val expiry = rate.calendar.addBusinessPeriod(market.t, expTenor)(using rate.bdConvention)
            val forward = rate.forward(expiry)
            val ks = ms.map(forward + _)
            expiry -> lib.Lazy(lib.VolatilitySkew(ks.toIndexedSeq, vs.toIndexedSeq))
      .flatMap: data =>
        buildMarketRate(currency, tenor).map: marketRate =>
          lib.VolatilitySurface(market.t, marketRate.forward, data.toIndexedSeq)

  def buildVolCube(currency: Currency): Either[lib.Error, lib.VolatilityCube[T]] =
    market.volCube(currency).flatMap: volCube =>
      val surfaces = volCube.cube.toList.traverse: (tenor, _) =>
        buildVolSurface(currency, tenor).tupleLeft(Tenor(tenor))
      .map(_.toIndexedSeq)
      val forwards = volCube.cube.keys.toList.traverse: tenor =>
        buildMarketRate(currency, tenor).map(_.forward).tupleLeft(Tenor(tenor))
      .map(_.toMap)
      (surfaces, forwards).tupled.map: (surfaces, forwards) =>
        lib.VolatilityCube[T](surfaces, forwards)

  def buildMarketRate(currency: Currency, tenor: Tenor): Either[lib.Error, lib.Underlying[T]] =
    market.volatilityConventions(currency, tenor).flatMap:
      case libor: dtos.Underlying.Libor[T]             => buildLibor(libor.name)
      case swap: dtos.Underlying.SwapRate[T]           => buildSwapRate(swap.name)
      case swap: dtos.Underlying.CompoundedSwapRate[T] => buildCompoundedSwapRate(swap.name)

  def buildLibor(rate: String): Either[lib.Error, lib.Libor[T]] =
    market.rate(rate).flatMap:
      case libor: dtos.Underlying.Libor[T] =>
        buildYieldCurve(libor.resetCurve).map: resetCurve =>
          new lib.Libor[T](
            libor.currency,
            Tenor(libor.tenor),
            libor.spotLag,
            toDayCounter(libor.dayCounter),
            buildCalendar(libor.calendar),
            resetCurve,
            libor.bdConvention
          )
      case _ => lib.Error.Generic(s"$rate is not a libor").asLeft

  def buildSwapRate(rate: String): Either[lib.Error, lib.SwapRate[T]] =
    market.rate(rate).flatMap:
      case swapRate: dtos.Underlying.SwapRate[T] =>
        buildYieldCurve(swapRate.discountCurve).flatMap: discountCurve =>
          buildLibor(swapRate.floatingRate).map: liborRate =>
            new lib.SwapRate[T](
              swapRate.tenor,
              swapRate.spotLag,
              swapRate.paymentDelay,
              Tenor(swapRate.fixedPeriod),
              liborRate,
              toDayCounter(swapRate.fixedDayCounter),
              buildCalendar(swapRate.calendar),
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
          buildLibor(libor.floatingRate).map: liborRate =>
            new lib.CompoundedSwapRate[T](
              libor.tenor,
              libor.spotLag,
              libor.paymentDelay,
              Tenor(libor.fixedPeriod),
              liborRate,
              Tenor(libor.fixedPeriod),
              toDayCounter(libor.fixedDayCounter),
              buildCalendar(libor.calendar),
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
          caplet.optionType
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
            discountCurve
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
          caplet.direction
        )
