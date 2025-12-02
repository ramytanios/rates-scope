package lib

import scala.math.tanh

trait VolatilitySkew:

  def apply(strike: Double): Double

object VolatilitySkew:

  def apply(ks: IndexedSeq[Double], vs: IndexedSeq[Double]): VolatilitySkew =

    val spline = CubicSpline(ks, vs)

    val kMin = ks.head
    val kMax = ks.last
    val vL = vs.head
    val vR = vs.last

    val w = 1.0 / 3.0

    k =>
      if k <= kMin then
        val dL = spline.fstDerivative(kMin)
        if dL <= 0 then dL * k + (vL - dL * kMin)
        else vL * (1 + w * (tanh(dL / vL / w * (k - kMin))))
      else if k >= kMax then
        val dR = spline.fstDerivative(kMax)
        if dR >= 0 then dR * k + (vR - dR * kMax)
        else vR * (1 + w * tanh(dR / vR / w * (k - kMax)))
      else spline(k)

  def flat(vol: Double): VolatilitySkew = _ => vol
