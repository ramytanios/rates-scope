package lib

import scala.math.tanh
import scala.math.pow

trait VolatilitySkew:

  def apply(strike: Double): Double

  def fstDerivative(strike: Double): Double

  def sndDerivative(strike: Double): Double

object VolatilitySkew:

  def apply(ks: IndexedSeq[Double], vs: IndexedSeq[Double]): VolatilitySkew =

    val spline = CubicSpline(ks, vs)

    val kMin = ks.head
    val kMax = ks.last
    val vL = vs.head
    val vR = vs.last

    val w = 1.0 / 3.0

    new VolatilitySkew:

      def apply(k: Double): Double =
        if k <= kMin then
          val dL = spline.fstDerivative(kMin)
          if dL <= 0 then dL * k + (vL - dL * kMin)
          else vL * (1 + w * tanh(dL / vL / w * (k - kMin)))
        else if k >= kMax then
          val dR = spline.fstDerivative(kMax)
          if dR >= 0 then dR * k + (vR - dR * kMax)
          else vR * (1 + w * tanh(dR / vR / w * (k - kMax)))
        else spline(k)

      def fstDerivative(k: Double): Double =
        if k <= kMin then
          val dL = spline.fstDerivative(kMin)
          if dL <= 0 then dL
          else dL * (1.0 - pow(tanh(dL / vL / w * (k - kMin)), 2))
        else if k >= kMax then
          val dR = spline.fstDerivative(kMax)
          if dR >= 0 then dR
          else dR * (1.0 - pow(tanh(dR / vR / w * (k - kMax)), 2))
        else spline.fstDerivative(k)

      def sndDerivative(k: Double): Double =
        if k <= kMin then
          val dL = spline.fstDerivative(kMin)
          if dL <= 0 then 0.0
          else
            val t = tanh(dL / vL / w * (k - kMin))
            2 * pow(dL, 2) / vL / w * t * (t * t - 1)
        else if k >= kMax then
          val dR = spline.fstDerivative(kMax)
          if dR >= 0 then 0.0
          else
            val t = tanh(dR / vR / w * (k - kMax))
            2 * pow(dR, 2) / vR / w * t * (t * t - 1)
        else spline.sndDerivative(k)

  def flat(vol: Double): VolatilitySkew =

    new VolatilitySkew:

      def apply(strike: Double): Double = vol

      def fstDerivative(strike: Double): Double = 0.0

      def sndDerivative(strike: Double): Double = 0.0
