package lib

import lib.quantities.Tenor
import lib.utils.BinarySearch

import java.time.LocalDate
import scala.math.Ordering.Implicits.*

trait VolatilityCube:

  def apply(tenor: Tenor): VolatilitySurface

object VolatilityCube:

  def apply(
      surfaces: IndexedSeq[(Tenor, VolatilitySurface)],
      forwards: Map[Tenor, LocalDate => Double]
  ): VolatilityCube =

    require(
      surfaces.map(_(0).toYearFraction.toDouble).isStrictlyIncreasing,
      "surfaces must be order by tenor"
    )

    val tenorMin = surfaces.head(0)
    val tenorMax = surfaces.last(0)

    tenor =>
      t =>
        k =>
          val forward = forwards(tenor)(t)
          val m = forward - k
          if tenor < tenorMin then
            val surface = surfaces.head(1)
            surface(t)(forwards(tenorMin)(t) - m)
          else if tenor > tenorMax then
            val surface = surfaces.last(1)
            surface(t)(forwards(tenorMax)(t) - m)
          else
            surfaces.searchBy(_(0))(tenor) match
              case BinarySearch.Found(i) =>
                val (_tenor, surface) = surfaces(i)
                surface(t)(forwards(_tenor)(t) - m)
              case BinarySearch.InsertionLoc(i) =>
                val (tenorL, surfaceL) = surfaces(i - 1)
                val (tenorR, surfaceR) = surfaces(i)
                val w = (tenor.toYearFraction.toDouble - tenorL.toYearFraction.toDouble) /
                  (tenorR.toYearFraction.toDouble - tenorL.toYearFraction.toDouble)
                (1 - w) * surfaceL(t)(forwards(tenorL)(t) - m) +
                  w * surfaceR(t)(forwards(tenorR)(t) - m)

  def flat(vol: Double): VolatilityCube = _ => VolatilitySurface.flat(vol)
