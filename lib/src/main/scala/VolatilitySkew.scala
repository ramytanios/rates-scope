package lib

import scala.math.tanh

trait VolatilitySkew:

  def apply(strike: Double): Double

object VolatilitySkew:

  def apply(ks: IndexedSeq[Double], vs: IndexedSeq[Double]): VolatilitySkew =
    val spline = CubicSpline.natural(ks, vs)

    val kMin = ks.head
    val kMax = ks.last
    val vL = vs.head
    val vR = vs.last

    val w = 1.0 / 3.0

    new VolatilitySkew:
      def apply(strike: Double): Double =
        if strike <= kMin then
          val dL = spline.fstDerivative(kMin)
          if dL <= 0 then dL * strike + (vL - dL * kMin)
          else vL * (1 + w * (tanh(dL / vL / w * (strike - kMin))))
        else if strike >= kMax then
          val dR = spline.fstDerivative(kMax)
          if dR >= 0 then dR * strike + (vR - dR * kMax)
          else vR * (1 + w * tanh(dR / vR / w * (strike - kMax)))
        else spline(strike)

  def flat(v: Double): VolatilitySkew = (_: Double) => v
