package entry

import lib.dtos.*
import cats.syntax.all.*
import lib.quantities.Tenor
import lib.VolatilitySurface
import lib.YieldCurve
import dtos.Calendar

class Builders[T: lib.DateLike](market: Market[T]):

  def yieldCurve(curve: dtos.Curve): Either[MarketError, YieldCurve[T]] =
    market.yieldCurve(curve).map: c =>
      lib.YieldCurve(market.t, c.discounts.toIndexedSeq)

  def calendar(cal: dtos.Calendar[T]): lib.Calendar[T] =
    lib.Calendar.fromHolidays(cal.holidays)

  def calFromUdl(udl: dtos.Underlying[T]): (Calendar[T], BusinessDayConvention) =
    udl match
      case l: dtos.Underlying.Libor[T]              => l.calendar -> l.bdConvention
      case s: dtos.Underlying.SwapRate[T]           => s.calendar -> s.bdConvention
      case s: dtos.Underlying.CompoundedSwapRate[T] => s.calendar -> s.bdConvention

  def volSurface(
      currency: Currency,
      tenor: lib.quantities.Tenor,
      fwd: lib.Forward[T]
  ): Either[MarketError, VolatilitySurface[T]] =
    market.volSurface(currency, tenor).flatMap: surf =>
      surf.surface.toList.traverse: (_tenor, _skew) =>
        val (ks, vs) = _skew.skew.unzip
        market.volatilityConventions(currency, _tenor).map: udl =>
          val (cal, bdConvention) = calFromUdl(udl)
          val mat = calendar(cal).addBusinessPeriod(market.t, _tenor)(using bdConvention)
          mat -> lib.Lazy(lib.VolatilitySkew(ks.toIndexedSeq, vs.toIndexedSeq))
      .map: data =>
        lib.VolatilitySurface(market.t, fwd, data.toIndexedSeq)

  def fromDayCounter(dc: dtos.DayCounter): lib.DayCounter =
    dc match
      case dtos.DayCounter.Act360 => lib.DayCounter.Act360
      case dtos.DayCounter.Act365 => lib.DayCounter.Act365

  def libor(rate: String): Either[lib.Error, lib.Libor[T]] = market.rate(rate).flatMap:
    case l: dtos.Underlying.Libor[T] =>
      yieldCurve(l.resetCurve).map: resetCurve =>
        new lib.Libor[T](
          l.currency,
          Tenor(l.tenor),
          l.spotLag,
          fromDayCounter(l.dayCounter),
          calendar(l.calendar),
          resetCurve,
          l.bdConvention
        )
    case _ => lib.Error.Generic(s"$rate is not a libor").asLeft

  def caplet(c: dtos.Payoff.Caplet[T]): Either[lib.Error, lib.Caplet[T]] =
    libor(c.rate).flatMap: l =>
      yieldCurve(c.discountCurve).map: discountCurve =>
        new lib.Caplet[T](
          l,
          c.fixingAt,
          c.startAt,
          c.endAt,
          c.paymentAt,
          c.paymentCurrency,
          c.strike,
          discountCurve,
          c.optionType
        )
