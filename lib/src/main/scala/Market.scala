package lib

import scala.collection.immutable.HashMap
import java.time.LocalDate

enum MissingMarketError(msg: String) extends Error(msg):

  case YieldCurve(ccy: Currency, name: String)
      extends MissingMarketError(s"missing curve $name in ccy $ccy")

  case Fixing(underlying: String)
      extends MissingMarketError(s"missing fixings of $underlying")

case class Curve(ccy: Currency, name: String)

case class Fixing(date: LocalDate, value: Double)

trait Market:

  def ref: LocalDate

  def yieldCurve(curve: Curve): Either[MissingMarketError, YieldCurve]

  def fixings(rate: String): Either[MissingMarketError, Seq[Fixing]]

object Market:

  def apply(
      _ref: LocalDate,
      _curves: HashMap[Curve, YieldCurve],
      _fixings: HashMap[String, Seq[Fixing]]
  ) =
    new Market:

      def ref: LocalDate = _ref

      def yieldCurve(curve: Curve): Either[MissingMarketError, YieldCurve] =
        _curves.get(curve).toRight(MissingMarketError.YieldCurve(curve.ccy, curve.name))

      def fixings(rate: String): Either[MissingMarketError, Seq[Fixing]] =
        _fixings.get(rate).toRight(MissingMarketError.Fixing(rate))
