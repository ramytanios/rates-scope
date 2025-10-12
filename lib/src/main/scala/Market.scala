package lib

import scala.collection.immutable.HashMap

enum MissingMarketError(msg: String) extends Error(msg):
  case YieldCurve(ccy: Currency, name: String)
      extends MissingMarketError(s"missing curve $name in ccy $ccy")

case class Curve(ccy: Currency, name: String)

trait Market:

  def yieldCurve(curve: Curve): Either[MissingMarketError, YieldCurve]

object Market:

  def fromMaps(curves: HashMap[Curve, YieldCurve]) =
    new Market:
      def yieldCurve(curve: Curve): Either[MissingMarketError, YieldCurve] =
        curves.get(curve).toRight(MissingMarketError.YieldCurve(curve.ccy, curve.name))
