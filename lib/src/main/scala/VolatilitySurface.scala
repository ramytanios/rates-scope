package lib

import lib.utils.BinarySearch

import java.time.LocalDate
import scala.math.Ordering.Implicits.*

import math.sqrt

trait VolatilitySurface:

  def apply(maturity: LocalDate): VolatilitySkew

object VolatilitySurface:

  def apply(
      refDate: LocalDate,
      forward: LocalDate => Double,
      skews: IndexedSeq[(LocalDate, VolatilitySkew)]
  ): VolatilitySurface =
    require(skews.map(_(0)).isStrictlyIncreasing, "pillar maturities must be strictly increasing")

    val t0 = refDate

    val tMin = skews.head(0)
    val tMax = skews.last(0)

    t =>
      k =>
        val m = forward(t) - k

        // linear interpolation in variance
        def interpolateBetween(
            s0: (LocalDate, VolatilitySkew),
            s1: (LocalDate, VolatilitySkew)
        ) =
          val (tL, skewL) = s0
          val (tR, skewR) = s1
          val w = Act365(tL, t) / Act365(tL, tR)
          val dt = Act365(t0, t).toDouble
          val dtL = Act365(t0, tL).toDouble
          val dtR = Act365(t0, tR).toDouble
          sqrt:
            1.0 / dt * (
              (1.0 - w) * skewL(forward(tL) - m) * dtL +
                w * skewR(forward(tR) - m) * dtR
            )

        if t < tMin then
          skews.head(1)(forward(tMin) - m)
        else if t > tMax then
          interpolateBetween(skews(0), skews(1))
        else
          skews.searchBy(_(0))(t) match
            case BinarySearch.Found(i) =>
              val (ti, skew) = skews(i)
              skew(forward(ti) - m)
            case BinarySearch.InsertionLoc(i) =>
              interpolateBetween(skews(i - 1), skews(i))

  def flat(vol: Double): VolatilitySurface = _ => VolatilitySkew.flat(vol)
