package lib

import lib.syntax.{ *, given }
import lib.utils.BinarySearch

import math.sqrt

trait VolatilitySurface[T]:

  def apply(maturity: T): VolatilitySkew

object VolatilitySurface:

  def apply[T: DateLike](
      ref: T,
      forward: Forward[T],
      skews: IndexedSeq[(T, Lazy[VolatilitySkew])]
  ): VolatilitySurface[T] =
    require(skews.map(_(0)).isStrictlyIncreasing, "pillar maturities must be strictly increasing")

    val t0 = ref
    val n = skews.length

    val tMin = skews.head(0)
    val tMax = skews.last(0)

    given DayCounter = DayCounter.Act365

    t =>
      k =>
        val m = forward(t) - k

        // linear interpolation in variance
        def interpolateBetween(s0: (T, VolatilitySkew), s1: (T, VolatilitySkew)) =
          val (tL, skewL) = s0
          val (tR, skewR) = s1
          val w = tL.yearFractionTo(t) / tL.yearFractionTo(tR)
          val dt = t0.yearFractionTo(t).toDouble
          val dtL = t0.yearFractionTo(tL).toDouble
          val dtR = t0.yearFractionTo(tR).toDouble
          sqrt:
            1.0 / dt * (
              (1.0 - w) * skewL(forward(tL) - m) * dtL +
                w * skewR(forward(tR) - m) * dtR
            )

        if t < tMin || skews.size == 1 then
          skews.head(1).value(forward(tMin) - m)
        else if t > tMax then
          interpolateBetween(
            skews(n - 2)(0) -> skews(n - 2)(1).value,
            skews(n - 1)(0) -> skews(n - 1)(1).value
          )
        else
          skews.searchBy(_(0))(t) match
            case BinarySearch.Found(i) =>
              val (ti, si) = skews(i)
              si.value(forward(ti) - m)
            case BinarySearch.InsertionLoc(i) =>
              interpolateBetween(
                skews(i - 1)(0) -> skews(i - 1)(1).value,
                skews(i)(0) -> skews(i)(1).value
              )

  def flat[T](vol: Double): VolatilitySurface[T] = _ => VolatilitySkew.flat(vol)

  def fromMoneynessSkew[T](
      forward: Forward[T],
      moneynesses: Seq[Double],
      vols: Seq[Double]
  ): VolatilitySurface[T] = new VolatilitySurface[T]:
    def apply(maturity: T): VolatilitySkew =
      val f = forward(maturity)
      val strikes = moneynesses.map(_ + f)
      VolatilitySkew(strikes.toIndexedSeq, vols.toIndexedSeq)
