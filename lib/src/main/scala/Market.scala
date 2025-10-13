package lib

import java.time.LocalDate

enum MarketError(msg: String) extends Error(msg):

  case YieldCurve(ccy: Currency, name: String)
      extends MarketError(s"missing curve $name in ccy $ccy")

  case FixingOf(underlying: String)
      extends MarketError(s"missing fixings of $underlying")

  case FixingAt(underlying: String, at: LocalDate)
      extends MarketError(s"missing fixing of $underlying at $at")

case class Curve(ccy: Currency, name: String)

case class Fixing(date: LocalDate, value: Double)

trait Market:

  def ref: LocalDate

  def yieldCurve(curve: Curve): Either[MarketError, YieldCurve]

  def fixings(rate: String): Either[MarketError, LocalDate => Either[MarketError, Fixing]]

object Market:

  def apply(
      refDate: LocalDate,
      curves: Map[Curve, YieldCurve],
      fixingsByRate: Map[String, Seq[Fixing]]
  ) =
    new Market:

      def ref: LocalDate = refDate

      def yieldCurve(curve: Curve): Either[MarketError, YieldCurve] =
        curves.get(curve).toRight(MarketError.YieldCurve(curve.ccy, curve.name))

      def fixings(rate: String): Either[MarketError, LocalDate => Either[MarketError, Fixing]] =
        fixingsByRate.get(rate)
          .toRight(MarketError.FixingOf(rate))
          .map: fixings =>
            val map = fixings.groupBy(_.date)
            (at: LocalDate) =>
              map.get(at).flatMap(_.headOption).toRight(MarketError.FixingAt(rate, at))
