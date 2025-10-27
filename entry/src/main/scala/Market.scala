package entry

import lib.*
import lib.quantities.*

enum MarketError(msg: String) extends Error(msg):

  case Rate(name: String)
      extends MarketError(s"missing rate $name")

  case YieldCurve(ccy: Currency, name: String)
      extends MarketError(s"missing curve $name in ccy $ccy")

  case FixingOf(underlying: String)
      extends MarketError(s"missing fixings of $underlying")

  case FixingAt[T](underlying: String, at: T)
      extends MarketError(s"missing fixing of $underlying at $at")

  case MarketRate(currency: Currency, tenor: Tenor)
      extends MarketError(s"missing market rate of tenor $tenor in currency $currency")

  case Volatility(currency: Currency, tenor: Tenor)
      extends MarketError(s"missing vol surface in currency $currency and tenor $tenor")

trait Market[T]:

  def t: T

  def rate(r: data.Rate): Either[MarketError, data.Underlying[T]]

  def yieldCurve(curve: data.Curve): Either[MarketError, data.YieldCurve[T]]

  def fixings(rate: String): Either[MarketError, T => Either[MarketError, data.Fixing[T]]]

  def volatilityConventions(currency: Currency, tenor: Tenor): Either[MarketError, data.Underlying[T]]

  def volSurface(currency: Currency, tenor: Tenor): Either[MarketError, data.VolatilitySurface[T]]

object Market:

  def apply[T](
      ref: T,
      rates: Map[data.Rate, data.Underlying[T]],
      curves: Map[data.Curve, data.YieldCurve[T]],
      fixingsByRate: Map[String, Seq[data.Fixing[T]]],
      volConventions: Map[Currency, Map[Tenor, data.Underlying[T]]],
      volatilities: Map[Currency, data.VolatilityCube[T]]
  ): Market[T] = new Market[T]:

    def t: T = ref

    def rate(r: data.Rate): Either[MarketError, data.Underlying[T]] =
      rates.get(r).toRight(MarketError.Rate(r.name))

    def yieldCurve(curve: data.Curve): Either[MarketError, data.YieldCurve[T]] =
      curves.get(curve).toRight(MarketError.YieldCurve(curve.ccy, curve.name))

    def fixings(rate: String): Either[MarketError, T => Either[MarketError, data.Fixing[T]]] =
      fixingsByRate.get(rate)
        .toRight(MarketError.FixingOf(rate))
        .map: fixings =>
          val map = fixings.groupBy(_.t)
          (at: T) =>
            map.get(at).flatMap(_.headOption).toRight(MarketError.FixingAt(rate, at))

    def volatilityConventions(
        currency: Currency,
        tenor: Tenor
    ): Either[MarketError, data.Underlying[T]] =
      volConventions.get(currency)
        .flatMap(_.get(tenor))
        .toRight(MarketError.MarketRate(currency, tenor))

    def volSurface(currency: Currency, tenor: Tenor): Either[MarketError, data.VolatilitySurface[T]] =
      volatilities.get(currency).map(_.cube(tenor)).toRight(MarketError.Volatility(currency, tenor))
