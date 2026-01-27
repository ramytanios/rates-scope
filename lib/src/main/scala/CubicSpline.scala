package lib

import org.apache.commons.math3.analysis.interpolation.SplineInterpolator

import scala.math.*

trait CubicSpline:

  def apply(x: Double): Double

  def fstDerivative(x: Double): Double

  def sndDerivative(x: Double): Double

object CubicSpline:

  def ofJava(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): CubicSpline =
    val m = xs.length
    require(xs.isStrictlyIncreasing, "xs must be strictly increasing")
    require(m == ys.length, "xs and ys size mismatch")
    require(m > 3, "need at least 3 points")

    val xMin = xs.head
    val xMax = xs.last

    val spline = SplineInterpolator().interpolate(xs.toArray, ys.toArray)

    new CubicSpline:

      override def apply(x: Double): Double =
        if x <= xMin then
          val d = this.fstDerivative(xMin)
          d * (x - xMin) + ys.head
        else if x >= xMax then
          val d = this.fstDerivative(xMax)
          d * (x - xMax) + ys.last
        else spline.value(x)

      override def fstDerivative(x: Double): Double =
        spline.derivative.value(min(max(x, xMin), xMax))

      override def sndDerivative(x: Double): Double =
        if x <= xMin || x >= xMax then 0.0
        else spline.polynomialSplineDerivative.derivative.value(x)

  def apply(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): CubicSpline =

    require(xs.zip(xs.tail).forall(_ < _), "x values must be stricly increasing")
    require(xs.length == ys.length, "xs and ys must have same length")

    val m = xs.length
    val n = m - 1

    require(m > 2, "cannot fit fewer than 3 knots")
    require(ys.length == m, "x and y don't have same length")

    val a = ys
    val b = Array.ofDim[Double](n)
    val c = Array.ofDim[Double](n + 1)
    val d = Array.ofDim[Double](n)

    val h = (0 until n - 1).map(i =>
      3 * ((ys(i + 2) - ys(i + 1)) / (xs(i + 2) - xs(i + 1)) - (ys(i + 1) - ys(i)) / (xs(i + 1) - xs(
        i
      )))
    )

    val diag = (0 until n - 1).map(i => 2 * (xs(i + 2) - xs(i)))
    val ldiag = (0 until n - 2).map(i => xs(i + 2) - xs(i + 1))
    val udiag = (0 until n - 2).map(i => xs(i + 2) - xs(i + 1))

    val cInterior = Tdma.solve(ldiag.toArray, diag.toArray, udiag.toArray, h.toArray)

    for i <- 0 until n - 1 do
      c(i + 1) = cInterior(i)

    for i <- 0 until n do
      b(i) =
        (ys(i + 1) - ys(i)) / (xs(i + 1) - xs(i)) - (2 * c(i) + c(i + 1)) * (xs(i + 1) - xs(i)) / 3

    for i <- 0 until n do
      d(i) = (c(i + 1) - c(i)) / (xs(i + 1) - xs(i)) / 3

    val c0 = c.slice(0, c.length - 1)

    val a1 = ys(0) +: a :+ ys(n)
    val b1 = b(0) +: b :+ b(n - 1) + c0(n - 1) * (xs(n) - xs(n - 1))
    val c1 = 0.0 +: c0 :+ 0.0
    val d1 = 0.0 +: d :+ 0.0

    new CubicSpline:

      def apply(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(max(0, i - 1))
        ((d1(i) * dx + c1(i)) * dx + b1(i)) * dx + a1(i)

      def fstDerivative(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(max(0, i - 1))
        (3 * d1(i) * dx + 2 * c1(i)) * dx + b1(i)

      def sndDerivative(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(max(0, i - 1))
        6 * d1(i) * dx + 2 * c1(i)
