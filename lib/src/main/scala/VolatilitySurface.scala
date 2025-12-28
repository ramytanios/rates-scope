package lib

import lib.syntax.{ *, given }
import lib.utils.BinarySearch

import math.sqrt
import math.pow

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
      new VolatilitySkew:

        def V(left: (T, Double => Double), right: (T, Double => Double), m: Double) =
          val (tL, fL) = left
          val (tR, fR) = right
          val w = tL.yearFractionTo(t) / tL.yearFractionTo(tR)
          val dt = t0.yearFractionTo(t).toDouble
          val dtL = t0.yearFractionTo(tL).toDouble
          val dtR = t0.yearFractionTo(tR).toDouble
          1.0 / dt * (
            (1.0 - w) * fL(forward(tL) - m) * dtL +
              w * fR(forward(tR) - m) * dtR
          )

        def apply(k: Double): Double =
          val m = forward(t) - k
          if t < tMin || skews.size == 1 then
            skews.head(1).value(forward(tMin) - m)
          else if t > tMax then
            sqrt:
              V(
                skews(n - 2)(0) -> skews(n - 2)(1).value.apply,
                skews(n - 1)(0) -> skews(n - 1)(1).value.apply,
                m
              )
          else
            skews.searchBy(_(0))(t) match
              case BinarySearch.Found(i) =>
                val (ti, si) = skews(i)
                si.value(forward(ti) - m)
              case BinarySearch.InsertionLoc(i) =>
                sqrt:
                  V(
                    skews(i - 1)(0) -> skews(i - 1)(1).value.apply,
                    skews(i)(0) -> skews(i)(1).value.apply,
                    m
                  )

        def fstDerivative(k: Double): Double =
          val m = forward(t) - k
          if t < tMin || skews.size == 1 then
            skews.head(1).value.fstDerivative(forward(tMin) - m)
          else if t > tMax then
            V(
              skews(n - 2)(0) -> skews(n - 2)(1).value.fstDerivative,
              skews(n - 1)(0) -> skews(n - 1)(1).value.fstDerivative,
              m
            ) / 2.0 / sqrt:
              V(
                skews(n - 2)(0) -> skews(n - 2)(1).value.apply,
                skews(n - 1)(0) -> skews(n - 1)(1).value.apply,
                m
              )
          else
            skews.searchBy(_(0))(t) match
              case BinarySearch.Found(i) =>
                val (ti, si) = skews(i)
                si.value.fstDerivative(forward(ti) - m)
              case BinarySearch.InsertionLoc(i) =>
                V(
                  skews(i - 1)(0) -> skews(i - 1)(1).value.fstDerivative,
                  skews(i)(0) -> skews(i)(1).value.fstDerivative,
                  m
                ) / 2.0 / sqrt:
                  V(
                    skews(i - 1)(0) -> skews(i - 1)(1).value.apply,
                    skews(i)(0) -> skews(i)(1).value.apply,
                    m
                  )

        def sndDerivative(k: Double): Double =
          val m = forward(t) - k

          if t < tMin || skews.size == 1 then
            skews.head(1).value.sndDerivative(forward(tMin) - m)
          else if t > tMax then
            val L2 = V(
              skews(n - 2)(0) -> skews(n - 2)(1).value.sndDerivative,
              skews(n - 1)(0) -> skews(n - 1)(1).value.sndDerivative,
              m
            )
            val L1 = V(
              skews(n - 2)(0) -> skews(n - 2)(1).value.fstDerivative,
              skews(n - 1)(0) -> skews(n - 1)(1).value.fstDerivative,
              m
            )
            val L = V(
              skews(n - 2)(0) -> skews(n - 2)(1).value.apply,
              skews(n - 1)(0) -> skews(n - 1)(1).value.apply,
              m
            )
            L2 / 2.0 / sqrt(L) - pow(L1, 2) / 4.0 / L / sqrt(L)
          else
            skews.searchBy(_(0))(t) match
              case BinarySearch.Found(i) =>
                val (ti, si) = skews(i)
                si.value.sndDerivative(forward(ti) - m)
              case BinarySearch.InsertionLoc(i) =>
                val L2 = V(
                  skews(i - 1)(0) -> skews(i - 1)(1).value.sndDerivative,
                  skews(i)(0) -> skews(i)(1).value.sndDerivative,
                  m
                )
                val L1 = V(
                  skews(i - 1)(0) -> skews(i - 1)(1).value.fstDerivative,
                  skews(i)(0) -> skews(i)(1).value.fstDerivative,
                  m
                )
                val L = V(
                  skews(i - 1)(0) -> skews(i - 1)(1).value.apply,
                  skews(i)(0) -> skews(i)(1).value.apply,
                  m
                )
                L2 / 2.0 / sqrt(L) - pow(L1, 2) / 4.0 / L / sqrt(L)

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
