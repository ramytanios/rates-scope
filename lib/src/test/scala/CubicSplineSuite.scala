package lib

import cats.syntax.all.*

class CubicSplineSuite extends munit.FunSuite:

  private case class Case(xs: Vector[Double], ys: Vector[Double])

  private val tol = 1e-12

  private val randCase: Rand[Case] =
    for
      n <- Rand.between(3, 50)
      x0 <- Rand.normal
      xs <- Rand.normal.listOfN(n).map(_.map(_.abs).scanLeft(x0)(_ + _))
      y0 <- Rand.normal
      ys <- Rand.normal.listOfN(n).map(_.scanLeft(y0)(_ + _))
    yield Case(xs.toVector, ys.toVector)

  test("should match value at knots"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case (Case(xs, ys), i) =>
          val interp = CubicSpline.natural(xs, ys)
          (xs zip ys).foreach: (x, y) =>
            assertEqualsDouble(interp(x), y, tol, s"i=$i, x=$x, y=$y")

  test("should be continuous in value, first and second derivatives"):
    throw NotImplementedError()

  test("should be linear in extrapolation region"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case (Case(xs, ys), i) =>
          val clue = s"i=$i"
          val spline = CubicSpline.natural(xs, ys)
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
