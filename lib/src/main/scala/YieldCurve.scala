package lib

import java.time.LocalDate
import scala.math.exp
import scala.math.log
import lib.quantities.*

trait YieldCurve:

  def spotRate(t: LocalDate): Rate

  def discount(to: LocalDate): Double

  def discount(from: LocalDate, to: LocalDate): Double =
    discount(to) / discount(from)

object YieldCurve:

  def apply(ref: LocalDate, dfs: Seq[(LocalDate, Double)], dayCounter: DayCounter): YieldCurve =

    val ts = dfs.map(_(0))

    require((ts zip ts.tail).forall(_.isBefore(_)), s"pillars must be strictly increasing")
    require(dfs.forall((t, _) => t.isAfter(ref)), s"pillars must be strictly after ref $ref")
    require(dfs.forall((_, df) => df > 0.0), s"discount factors must be positive")

    val yfs = 0.0 +: ts.map(t => dayCounter.yearFraction(ref, t).toDouble).toIndexedSeq
    val rts = 0.0 +: dfs.map((_, df) => -log(df)).toIndexedSeq

    val interp = LinearInterpolation.withLinearExtrapolation(yfs, rts)

    new YieldCurve:
      def spotRate(t: LocalDate): Rate =
        val yf = dayCounter.yearFraction(ref, t)
        interp(yf.toDouble) / yf.toDouble

      def discount(to: LocalDate): Double =
        val yf = dayCounter.yearFraction(ref, to)
        val rt = interp(yf.toDouble)
        exp(-rt)

  def continousCompounding(ref: LocalDate, rate: Rate, dayCounter: DayCounter): YieldCurve =
    new YieldCurve:
      def spotRate(t: LocalDate): Rate = rate

      def discount(to: LocalDate): Double = discount(ref, to)

      override def discount(from: LocalDate, to: LocalDate): Double =
        val dt = dayCounter.yearFraction(from, to)
        exp(-rate * dt)

  def zero: YieldCurve =
    new YieldCurve:
      def spotRate(t: LocalDate): Rate = Rate(0.0)

      def discount(to: LocalDate): Double = 1.0

      override def discount(from: LocalDate, to: LocalDate): Double = 1.0
