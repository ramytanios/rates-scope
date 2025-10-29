package entry

import lib.dtos.Currency
import lib.quantities.*

enum MarketError(msg: String) extends lib.Error(msg):

  case Rate(name: String)
      extends MarketError(s"missing rate $name")

  case YieldCurve(ccy: Currency, name: String)
      extends MarketError(s"missing curve $name in ccy $ccy")

  case FixingOf(underlying: String)
      extends MarketError(s"missing fixings of $underlying")

  case FixingAt[T](underlying: String, at: T)
      extends MarketError(s"missing fixing of $underlying at $at")

  case Volatility(currency: Currency, tenor: Tenor)
      extends MarketError(s"missing vol surface in currency $currency and tenor $tenor")

  case VolatilityConventions(currency: Currency, tenor: Tenor)
      extends MarketError(s"missing vol conventions in currency $currency and tenor $tenor")

trait Market[T]:

  def t: T

  def rate(name: String): Either[MarketError, dtos.Underlying[T]]

  def yieldCurve(curve: dtos.Curve): Either[MarketError, dtos.YieldCurve[T]]

  def fixings(rate: String): Either[MarketError, T => Either[MarketError, dtos.Fixing[T]]]

  def volatilityConventions(currency: Currency, tenor: Tenor): Either[MarketError, dtos.Underlying[T]]

  def volSurface(currency: Currency, tenor: Tenor): Either[MarketError, dtos.VolatilitySurface[T]]

object Market:

  def apply[T](
      ref: T,
      rates: Map[String, dtos.Underlying[T]],
      curves: Map[dtos.Curve, dtos.YieldCurve[T]],
      fixingsByRate: Map[String, Seq[dtos.Fixing[T]]],
      volConventions: Map[Currency, Map[Tenor, dtos.Underlying[T]]],
      volatilities: Map[Currency, dtos.VolatilityCube[T]]
  ): Market[T] = new Market[T]:

    def t: T = ref

    def rate(name: String): Either[MarketError, dtos.Underlying[T]] =
      rates.get(name).toRight(MarketError.Rate(name))

    def yieldCurve(curve: dtos.Curve): Either[MarketError, dtos.YieldCurve[T]] =
      curves.get(curve).toRight(MarketError.YieldCurve(curve.ccy, curve.name))

    def fixings(rate: String): Either[MarketError, T => Either[MarketError, dtos.Fixing[T]]] =
      fixingsByRate.get(rate)
        .toRight(MarketError.FixingOf(rate))
        .map: fixings =>
          val map = fixings.groupBy(_.t)
          (at: T) =>
            map.get(at).flatMap(_.headOption).toRight(MarketError.FixingAt(rate, at))

    def volatilityConventions(
        currency: Currency,
        tenor: Tenor
    ): Either[MarketError, dtos.Underlying[T]] =
      volConventions.get(currency).flatMap(_.get(tenor)).toRight(MarketError.VolatilityConventions(
        currency,
        tenor
      ))

    def volSurface(currency: Currency, tenor: Tenor): Either[MarketError, dtos.VolatilitySurface[T]] =
      volatilities.get(currency).map(_.cube(tenor.toPeriod)).toRight(MarketError.Volatility(
        currency,
        tenor
      ))
