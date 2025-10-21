package lib

import lib.quantities.*
import lib.syntax.{ *, given }

import scala.math.Ordering.Implicits.*
import scala.math.exp
import scala.math.log

trait YieldCurve[T]:

  def spotRate(t: T): Rate

  def discount(to: T): Double

  def discount(from: T, to: T): Double = discount(to) / discount(from)

object YieldCurve:

  def apply[T: TimeLike](
      ref: T,
      dfs: IndexedSeq[(T, Double)],
      dayCounter: DayCounter
  ): YieldCurve[T] =

    given DayCounter = dayCounter

    val ts = dfs.map(_(0))

    require(ts.isStrictlyIncreasing, s"pillars must be strictly increasing")
    require(dfs.forall((t, _) => t > ref), s"pillars must be strictly after ref $ref")
    require(dfs.forall((_, df) => df > 0.0), s"discount factors must be positive")

    val yfs = 0.0 +: ts.map(ref.yearFractionTo(_).toDouble)
    val rts = 0.0 +: dfs.map((_, df) => -log(df))

    val interp = LinearInterpolation.withLinearExtrapolation(yfs, rts)

    new YieldCurve[T]:
      def spotRate(t: T): Rate =
        val yf = ref.yearFractionTo(t)
        interp(yf.toDouble) / yf.toDouble
      def discount(to: T): Double =
        val yf = ref.yearFractionTo(to)
        val rt = interp(yf.toDouble)
        exp(-rt)

  def continousCompounding[T: TimeLike](
      ref: T,
      rate: Rate,
      dayCounter: DayCounter
  ): YieldCurve[T] =
    given DayCounter = dayCounter
    new YieldCurve:
      def spotRate(t: T): Rate = rate
      def discount(to: T): Double = discount(ref, to)
      override def discount(from: T, to: T): Double =
        val dt = from.yearFractionTo(to)
        exp(-rate * dt)

  def zero[T]: YieldCurve[T] =
    new YieldCurve[T]:
      def spotRate(t: T): Rate = Rate(0.0)
      def discount(to: T): Double = 1.0
      override def discount(from: T, to: T): Double = 1.0
