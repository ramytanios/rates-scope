package lib

import lib.quantities.Tenor

enum MarketError(msg: String) extends Error(msg):

  case YieldCurve(ccy: Currency, name: String)
      extends MarketError(s"missing curve $name in ccy $ccy")

  case FixingOf(underlying: String)
      extends MarketError(s"missing fixings of $underlying")

  case FixingAt[T](underlying: String, at: T)
      extends MarketError(s"missing fixing of $underlying at $at")

  case MarketRate(tenor: Tenor)
      extends MarketError(s"missing market rate of tenor $tenor")

case class Curve(ccy: Currency, name: String)

case class Fixing[T](t: T, value: Double)

trait Market[T]:

  def ref: T

  def yieldCurve(curve: Curve): Either[MarketError, YieldCurve[T]]

  def fixings(rate: String): Either[MarketError, T => Either[MarketError, Fixing[T]]]

  def marketRates(tenor: Tenor): Either[MarketError, Underlying[T]]

object Market:

  def apply[T](
      refDate: T,
      curves: Map[Curve, YieldCurve[T]],
      fixingsByRate: Map[String, Seq[Fixing[T]]],
      marketRatesByTenor: Map[Tenor, Underlying[T]]
  ) =
    new Market[T]:

      def ref: T = refDate

      def yieldCurve(curve: Curve): Either[MarketError, YieldCurve[T]] =
        curves.get(curve).toRight(MarketError.YieldCurve(curve.ccy, curve.name))

      def fixings(rate: String): Either[MarketError, T => Either[MarketError, Fixing[T]]] =
        fixingsByRate.get(rate)
          .toRight(MarketError.FixingOf(rate))
          .map: fixings =>
            val map = fixings.groupBy(_.t)
            (at: T) =>
              map.get(at).flatMap(_.headOption).toRight(MarketError.FixingAt(rate, at))

      def marketRates(tenor: Tenor): Either[MarketError, Underlying[T]] =
        marketRatesByTenor.get(tenor).toRight(MarketError.MarketRate(tenor))

  def fromSingleCurve[T](ref: T, currency: Currency, name: String, curve: YieldCurve[T]) =
    apply[T](ref, Map(Curve(currency, name) -> curve), Map.empty, Map.empty)
