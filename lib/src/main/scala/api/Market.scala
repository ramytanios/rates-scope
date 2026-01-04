package lib.api

import lib.*
import lib.dtos
import lib.quantities.*

enum MarketError(msg: String) extends lib.Error(msg):

  case MissingRate(name: String)
      extends MarketError(s"missing rate $name")

  case MissingYieldCurve(ccy: dtos.Currency, name: String)
      extends MarketError(s"missing curve $name in ccy $ccy")

  case MissingFixingOf(underlying: String)
      extends MarketError(s"missing fixings of $underlying")

  case MissingFixingAt[T](underlying: String, at: T)
      extends MarketError(s"missing fixing of $underlying at $at")

  case MissingVolatilityCube(currency: dtos.Currency)
      extends MarketError(s"missing vol cube of currency $currency")

  case MissingVolatilitySurface(currency: dtos.Currency, tenor: Tenor)
      extends MarketError(s"missing vol surface in currency $currency and tenor $tenor")

  case MissingVolatilityConventions(currency: dtos.Currency, tenor: Tenor)
      extends MarketError(s"missing vol conventions in currency $currency and tenor $tenor")

  case MissingCalendar(name: String) extends MarketError(s"missing calendar $name")

trait Market[T]:

  def t: T

  def rate(name: String): Either[MarketError, dtos.Underlying[T]]

  def yieldCurve(curve: dtos.Curve): Either[MarketError, dtos.YieldCurve[T]]

  def fixings(rate: String): Either[MarketError, Seq[dtos.Fixing[T]]]

  def volatilityConventions(
      currency: dtos.Currency,
      tenor: Tenor
  ): Either[MarketError, dtos.Underlying[T]]

  def volCube(currency: dtos.Currency): Either[MarketError, dtos.VolatilityCube]

  def volSurface(
      currency: dtos.Currency,
      tenor: Tenor
  ): Either[MarketError, dtos.VolatilitySurface]

  def calendar(name: String): Either[MarketError, dtos.Calendar[T]]

object Market:

  def apply[T](
      tRef: T,
      marketByCcy: Map[dtos.Currency, dtos.CcyMarket[T]],
      static: dtos.Static[T]
  ): Market[T] =
    this.apply(
      tRef,
      rates = marketByCcy.values.map(_.rates).reduce(_ ++ _),
      curves = marketByCcy.flatMap((ccy, market) =>
        market.curves.map((name, curve) => dtos.Curve(ccy, name) -> curve)
      ).toMap,
      fixingsByRate = marketByCcy.values.map(_.fixings).reduce(_ ++ _),
      volConventions = marketByCcy.view.mapValues(_.volConventions.rates
        .map((p, v) => (p: Tenor) -> v)).toMap,
      volatilities = marketByCcy.view.mapValues(_.volatility).toMap,
      calendars = static.calendars
    )

  def apply[T](
      tRef: T,
      rates: Map[String, dtos.Underlying[T]],
      curves: Map[dtos.Curve, dtos.YieldCurve[T]],
      fixingsByRate: Map[String, Seq[dtos.Fixing[T]]],
      volConventions: Map[dtos.Currency, Map[Tenor, dtos.Underlying[T]]],
      volatilities: Map[dtos.Currency, dtos.VolatilityCube],
      calendars: Map[String, dtos.Calendar[T]]
  ): Market[T] = new Market[T]:

    import MarketError.*

    def t: T = tRef

    def rate(name: String): Either[MarketError, dtos.Underlying[T]] =
      rates.get(name).toRight(MissingRate(name))

    def yieldCurve(curve: dtos.Curve): Either[MarketError, dtos.YieldCurve[T]] =
      curves.get(curve).toRight(MissingYieldCurve(curve.currency, curve.name))

    def fixings(rate: String): Either[MarketError, Seq[dtos.Fixing[T]]] =
      fixingsByRate.get(rate).toRight(MissingFixingOf(rate))

    def volatilityConventions(
        currency: dtos.Currency,
        tenor: Tenor
    ): Either[MarketError, dtos.Underlying[T]] =
      volConventions.get(currency).flatMap(_.get(tenor))
        .toRight(MissingVolatilityConventions(currency, tenor))

    def volCube(currency: dtos.Currency): Either[MarketError, dtos.VolatilityCube] =
      volatilities.get(currency).toRight(MissingVolatilityCube(currency))

    def volSurface(
        currency: dtos.Currency,
        tenor: Tenor
    ): Either[MarketError, dtos.VolatilitySurface] =
      volatilities.get(currency).map(_.cube(tenor.toPeriod))
        .toRight(MissingVolatilitySurface(currency, tenor))

    def calendar(name: String): Either[MarketError, dtos.Calendar[T]] =
      calendars.get(name).toRight(MissingCalendar(name))
