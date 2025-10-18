package lib

import java.time.LocalDate
import scala.math.Ordering.Implicits.*
import lib.algorithms.BinarySearch
import math.sqrt

trait VolatilitySurface:

  def apply(maturity: LocalDate): VolatilitySkew

object VolatilitySurface:

  def apply(
      refDate: LocalDate,
      skews: IndexedSeq[(LocalDate, VolatilitySkew)],
      forward: LocalDate => Double
  ): VolatilitySurface =

    require(skews.map(_(0)).isStrictlyIncreasing, "pillar maturities must be strictly increasing")

    maturity =>
      strike =>
        val matMin = skews.head(0)
        val matMax = skews.last(0)
        val moneyness = forward(maturity) - strike

        def linearInterp(s0: (LocalDate, VolatilitySkew), s1: (LocalDate, VolatilitySkew)) =
          val (matL, skewL) = s0
          val (matR, skewR) = s1
          val w = Act365.yearFraction(matL, maturity) / Act365.yearFraction(matL, matR)
          val dt = Act365.yearFraction(refDate, maturity).toDouble
          val dtL = Act365.yearFraction(refDate, matL).toDouble
          val dtR = Act365.yearFraction(refDate, matR).toDouble
          sqrt:
            1.0 / dt * (
              (1.0 - w) * skewL(forward(matL) - moneyness) * dtL +
                w * skewR(forward(matR) - moneyness) * dtR
            )

        if maturity < matMin then
          skews.head(1)(forward(matMin) - moneyness)
        else if maturity > matMax then
          linearInterp(skews(0), skews(1))
        else
          skews.searchBy(_(0))(maturity) match
            case BinarySearch.Found(i) =>
              val (mat, skew) = skews(i)
              skew(forward(mat) - moneyness)
            case BinarySearch.InsertionLoc(i) =>
              linearInterp(skews(i - 1), skews(i))

  def flat(vol: Double): VolatilitySurface = _ => VolatilitySkew.flat(vol)
