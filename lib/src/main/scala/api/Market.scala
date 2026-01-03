package lib.api

import lib.*
import lib.dtos
import lib.quantities.*

enum MarketError(msg: String) extends lib.Error(msg):

  case Rate(name: String)
      extends MarketError(s"missing rate $name")

  case YieldCurve(ccy: dtos.Currency, name: String)
      extends MarketError(s"missing curve $name in ccy $ccy")

  case FixingOf(underlying: String)
      extends MarketError(s"missing fixings of $underlying")

  case FixingAt[T](underlying: String, at: T)
      extends MarketError(s"missing fixing of $underlying at $at")

  case VolatilityCube(currency: dtos.Currency)
      extends MarketError(s"missing vol cube of currency $currency")

  case VolatilitySurface(currency: dtos.Currency, tenor: Tenor)
      extends MarketError(s"missing vol surface in currency $currency and tenor $tenor")

  case VolatilityConventions(currency: dtos.Currency, tenor: Tenor)
      extends MarketError(s"missing vol conventions in currency $currency and tenor $tenor")

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

object Market:

  def apply[T](
      tRef: T,
      rates: Map[String, dtos.Underlying[T]],
      curves: Map[dtos.Curve, dtos.YieldCurve[T]],
      fixingsByRate: Map[String, Seq[dtos.Fixing[T]]],
      volConventions: Map[dtos.Currency, Map[Tenor, dtos.Underlying[T]]],
      volatilities: Map[dtos.Currency, dtos.VolatilityCube]
  ): Market[T] = new Market[T]:

    def t: T = tRef

    def rate(name: String): Either[MarketError, dtos.Underlying[T]] =
      rates.get(name).toRight(MarketError.Rate(name))

    def yieldCurve(curve: dtos.Curve): Either[MarketError, dtos.YieldCurve[T]] =
      curves.get(curve).toRight(MarketError.YieldCurve(curve.ccy, curve.name))

    def fixings(rate: String): Either[MarketError, Seq[dtos.Fixing[T]]] =
      fixingsByRate.get(rate).toRight(MarketError.FixingOf(rate))

    def volatilityConventions(
        currency: dtos.Currency,
        tenor: Tenor
    ): Either[MarketError, dtos.Underlying[T]] =
      volConventions.get(currency).flatMap(_.get(tenor))
        .toRight(MarketError.VolatilityConventions(currency, tenor))

    def volCube(currency: dtos.Currency): Either[MarketError, dtos.VolatilityCube] =
      volatilities.get(currency).toRight(MarketError.VolatilityCube(currency))

    def volSurface(
        currency: dtos.Currency,
        tenor: Tenor
    ): Either[MarketError, dtos.VolatilitySurface] =
      volatilities.get(currency).map(_.cube(tenor.toPeriod))
        .toRight(MarketError.VolatilitySurface(currency, tenor))
