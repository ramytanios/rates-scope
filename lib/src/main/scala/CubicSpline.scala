package lib

trait CubicSpline:

  def apply(x: Double): Double

  def fstDerivative(x: Double): Double

  def sndDerivative(x: Double): Double

object CubicSpline:

  def natural: CubicSpline = throw NotImplementedError()
