package lib

import lib.syntax.{ *, given }
import lib.utils.BinarySearch

import math.sqrt
import math.pow

trait VolatilitySurface[T]:

  def apply(maturity: T): VolatilitySkew

object VolatilitySurface:

  def apply[T: DateLike](
      tRef: T,
      forward: Forward[T],
      skews: IndexedSeq[(T, Lazy[VolatilitySkew])]
  ): VolatilitySurface[T] =
    require(skews.map(_(0)).isStrictlyIncreasing, "pillar maturities must be strictly increasing")

    val n = skews.length

    val tMin = skews.head(0)
    val tMax = skews.last(0)

    given DayCounter = DayCounter.Act365

    t =>
      new VolatilitySkew:

        def L(left: (T, Double => Double), right: (T, Double => Double), m: Double) =
          val (tL, fL) = left
          val (tR, fR) = right
          val w = tL.yearFractionTo(t) / tL.yearFractionTo(tR)
          val dt = tRef.yearFractionTo(t).toDouble
          val dtL = tRef.yearFractionTo(tL).toDouble
          val dtR = tRef.yearFractionTo(tR).toDouble
          1.0 / dt * ((1.0 - w) * dtL * fL(forward(tL) - m) + w * dtR * fR(forward(tR) - m))

        def apply(k: Double): Double =

          val m = forward(t) - k

          val I = (k: Int) =>
            sqrt:
              L(
                skews(k - 1)(0) -> skews(k - 1)(1).value.apply,
                skews(k)(0) -> skews(k)(1).value.apply,
                m
              )

          if t < tMin || skews.size == 1 then
            skews.head(1).value(forward(tMin) - m)
          else if t > tMax then I(n - 1)
          else
            skews.searchBy(_(0))(t) match
              case BinarySearch.Found(i)        => skews(i)(1).value(k)
              case BinarySearch.InsertionLoc(i) => I(i)

        def fstDerivative(k: Double): Double =

          val m = forward(t) - k

          val I = (k: Int) =>
            L(
              skews(k - 1)(0) -> skews(k - 1)(1).value.fstDerivative,
              skews(k)(0) -> skews(k)(1).value.fstDerivative,
              m
            ) / 2.0 / sqrt:
              L(
                skews(k - 1)(0) -> skews(k - 1)(1).value.apply,
                skews(k)(0) -> skews(k)(1).value.apply,
                m
              )

          if t < tMin || skews.size == 1 then
            skews.head(1).value.fstDerivative(forward(tMin) - m)
          else if t > tMax then I(n - 1)
          else
            skews.searchBy(_(0))(t) match
              case BinarySearch.Found(i)        => skews(i)(1).value.fstDerivative(k)
              case BinarySearch.InsertionLoc(i) => I(i)

        def sndDerivative(k: Double): Double =

          val m = forward(t) - k

          val I = (k: Int) =>
            val a = L(
              skews(k - 1)(0) -> skews(k - 1)(1).value.sndDerivative,
              skews(k)(0) -> skews(k)(1).value.sndDerivative,
              m
            )
            val b = L(
              skews(k - 1)(0) -> skews(k - 1)(1).value.fstDerivative,
              skews(k)(0) -> skews(k)(1).value.fstDerivative,
              m
            )
            val c = L(
              skews(k - 1)(0) -> skews(k - 1)(1).value.apply,
              skews(k)(0) -> skews(k)(1).value.apply,
              m
            )
            a / 2.0 / sqrt(c) - pow(b, 2) / 4.0 / c / sqrt(c)

          if t < tMin || skews.size == 1 then
            skews.head(1).value.sndDerivative(forward(tMin) - m)
          else if t > tMax then I(n - 1)
          else
            skews.searchBy(_(0))(t) match
              case BinarySearch.Found(i)        => skews(i)(1).value.sndDerivative(k)
              case BinarySearch.InsertionLoc(i) => I(i)

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
