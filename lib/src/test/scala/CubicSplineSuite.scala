package lib

import cats.syntax.all.*

import scala.math.*

class CubicSplineSuite extends munit.FunSuite:

  private case class Case(xs: Vector[Double], ys: Vector[Double])

  private val tol = 1e-12

  private val randCase: Rand[Case] =
    for
      n <- Rand.between(3, 50)
      x0 <- Rand.normal
      xs <- Rand.normal.listOfN(n).map(_.map(_.abs + 0.01).scan(x0)(_ + _))
      y0 <- Rand.normal
      ys <- Rand.normal.listOfN(n).map(_.scan(y0)(_ + _))
    yield Case(xs.toVector, ys.toVector)

  test("values at knots"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case Case(xs, ys) -> i =>
          val interp = CubicSpline(xs, ys)
          (xs zip ys).foreach: (x, y) =>
            assertEqualsDouble(interp(x), y, tol, s"i=$i, x=$x, y=$y")

  test("derivatives values against the java implementation"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case Case(xs, ys) -> i =>
          val javaImpl = CubicSpline.ofJava(xs, ys)
          val customImpl = CubicSpline(xs, ys)
          xs.foreach: x =>
            assertEqualsDouble(
              customImpl.fstDerivative(x),
              javaImpl.fstDerivative(x),
              1e-8,
              s"i=$i, 1st deriv"
            )
            assertEqualsDouble(
              customImpl.sndDerivative(x),
              javaImpl.sndDerivative(x),
              1e-8,
              s"i=$i, 2nd deriv"
            )

  test("value, 1st and 2nd derivatives continuity"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach: (c, i) =>
        val spline = CubicSpline(c.xs.toIndexedSeq, c.ys.toIndexedSeq)
        val xRange = c.xs.max - c.xs.min
        val yRange = c.ys.max - c.ys.min
        val dRange = c.xs.map(spline.fstDerivative).max - c.xs.map(spline.fstDerivative).min
        val d2Range = c.xs.map(spline.sndDerivative).max - c.xs.map(spline.sndDerivative).min

        val eps0 = 1e-4 * xRange
        val epsMin = 1e-11 * xRange
        (c.xs zip c.ys).zipWithIndex.foreach:
          case x -> y -> j =>
            val passValue = Iterator
              .unfold(eps0): eps =>
                val yLeft = spline(x - eps)
                val yRight = spline(x + eps)
                val delta = max(abs(yLeft - y), abs(yRight - y))
                if eps < epsMin then None else Some(delta, eps / 2)
              .exists(_ < 1e-5 * yRange)

            val passFstDeriv = Iterator
              .unfold(eps0): eps =>
                val d = spline.fstDerivative(x)
                val dLeft = spline.fstDerivative(x - eps)
                val dRight = spline.fstDerivative(x + eps)
                val delta = max(abs(dLeft - d), abs(dRight - d))
                if eps < epsMin then None else Some(delta, eps / 2)
              .exists(_ < 1e-5 * dRange)

            val passSndDeriv = Iterator
              .unfold(eps0): eps =>
                val d2 = spline.sndDerivative(x)
                val d2Left = spline.sndDerivative(x - eps)
                val d2Right = spline.sndDerivative(x + eps)
                val delta = max(abs(d2Left - d2), abs(d2Right - d2))
                if eps < epsMin then None else Some(delta, eps / 2)
              .exists(_ < 1e-5 * d2Range)

            assert(passValue, s"i=$i, value")
            assert(passFstDeriv, s"i=$i, j=$j, 1st deriv, dRange=$dRange")
            assert(passSndDeriv, s"i=$i, j=$j, 2nd deriv, d2Range=$dRange")

  test("linear extrapolation"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case Case(xs, ys) -> i =>
          val clue = s"i=$i"
          val spline = CubicSpline(xs, ys)
          val xMin = xs.head
          val xMax = xs.last
          val xRange = xMax - xMin
          val dx = 0.1 * xRange
          val x1 = xMin - dx
          val x2 = xMin - 2 * dx
          val d1 = spline.fstDerivative(x1)
          val d2 = spline.fstDerivative(x2)
          val x3 = xMax + dx
          val x4 = xMax + 2 * dx
          val d3 = spline.fstDerivative(x3)
          val d4 = spline.fstDerivative(x4)
          assertEqualsDouble(d1, d2, tol, clue)
          assertEqualsDouble(spline.sndDerivative(x1), 0.0, tol, clue)
          assertEqualsDouble(d3, d4, tol, clue)
          assertEqualsDouble(spline.sndDerivative(x3), 0.0, tol, clue)
