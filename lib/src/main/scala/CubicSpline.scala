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

    val n = m - 1 // n intervals, n + 1 points

    val xMin = xs.head
    val xMax = xs.last

    val spline = SplineInterpolator().interpolate(xs.toArray, ys.toArray)

    new CubicSpline:

      override def apply(x: Double): Double =
        if x <= xMin then
          val d = this.fstDerivative(xMin)
          d * (x - xMin) + ys(0)
        else if x >= xMax then
          val d = this.fstDerivative(xMax)
          d * (x - xMax) + ys(n)
        else spline.value(x)

      override def fstDerivative(x: Double): Double =
        spline.derivative.value(min(max(x, xMin), xMax))

      override def sndDerivative(x: Double): Double =
        if x <= xMin || x >= xMax then 0.0
        else spline.polynomialSplineDerivative.derivative.value(x)

  def apply(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): CubicSpline =
    val m = xs.length
    require(xs.isStrictlyIncreasing, "xs must be strictly increasing")
    require(m == ys.length, "xs and ys size mismatch")
    require(m > 3, "need at least 3 points")

    val n = m - 1 // n intervals, n + 1 points

    val as = ys
    val bs = Array.ofDim[Double](n)
    val cs = Array.ofDim[Double](n + 1)
    val ds = Array.ofDim[Double](n)

    val ldiag = (0 until n - 2).map(i => xs(i + 2) - xs(i + 1))
    val diag = (0 until n - 1).map(i => 2 * xs(i + 2) - xs(i))
    val udiag = ldiag
    val rhs = (0 until n - 1).map(i =>
      3 * ((ys(i + 2) - ys(i + 1)) / (xs(i + 2) - xs(i + 1)) - (ys(i + 1) - ys(i)) / (xs(i + 1) - xs(
        i
      )))
    )

    val csInterior = Tdma.solve(ldiag.toArray, diag.toArray, udiag.toArray, rhs.toArray)

    for i <- (0 until n - 1) do
      cs(i + 1) = csInterior(i) 

    for i <- 0 until n do
      bs(i) =
        (ys(i + 1) - ys(i)) / (xs(i + 1) - xs(i)) - (2 * cs(i) + cs(i + 1)) * (xs(i + 1) - xs(i)) / 3

    for i <- 0 until n do
      ds(i) = (cs(i + 1) - cs(i)) / (xs(i + 1) - xs(i)) / 3

    val a = ys(0) +: as :+ ys(n)
    val b = bs(0) +: bs :+ bs( n - 1) + cs(n - 1) * (xs(n) - xs(n - 1)) 
    val c = 0.0 +: cs.slice(0, cs.length - 1) :+ 0.0
    val d = 0.0 +: ds :+ 0.0

    new CubicSpline:

      def apply(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(max(0, i - 1))
        a(i) + dx * (b(i) + dx * (c(i) + dx * d(i)))

      def fstDerivative(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(max(0, i - 1))
        b(i) + dx * (2 * c(i) + dx * 3 * d(i))

      def sndDerivative(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(max(0, i - 1))
        2 * c(i) + 6 * d(i) * dx
