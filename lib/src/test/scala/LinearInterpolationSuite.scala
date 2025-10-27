package lib

import cats.syntax.all.*

class LinearInterpolationSuite extends munit.FunSuite:

  private case class Case(xs: Vector[Double], ys: Vector[Double])

  private val tol = 1e-10

  private val randCase: Rand[Case] =
    for
      n <- Rand.between(2, 10)
      x0 <- Rand.normal
      xs <- Rand.normal.listOfN(n).map(_.map(_.abs).scanLeft(x0)(_ + _))
      y0 <- Rand.normal
      ys <- Rand.normal.listOfN(n).map(_.scanLeft(y0)(_ + _))
    yield Case(xs.toVector, ys.toVector)

  test("values at knots"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case (Case(xs, ys), i) =>
          val interp = LinearInterpolation.withLinearExtrapolation(xs, ys)
          (xs zip ys).foreach: (x, y) =>
            assertEqualsDouble(interp(x), y, tol, s"i=$i, x=$x, y=$y")

  test("linear extrapolation"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case (Case(xs, ys), i) =>
          val n = xs.size
          val interp = LinearInterpolation.withLinearExtrapolation(xs, ys)
          val xl = xs.head
          val xr = xs.last
          val xrange = xr - xl
          val step = 0.1 * xrange
          val yl = ys.head
          val yr = ys.last
          val sl = (ys(0) - ys(1)) / (xs(0) - xs(1))
          val sr = (ys(n - 1) - ys(n - 2)) / (xs(n - 1) - xs(n - 2))
          assertEqualsDouble(interp(xl - step), sl * (xl - step) + (yl - sl * xl), tol, s"i=$i")
          assertEqualsDouble(interp(xr + step), sr * (xr + step) + (yr - sr * xr), tol, s"i=$i")

  test("flat extrapolation"):
    randCase
      .view
      .take(10)
      .zipWithIndex
      .foreach:
        case (Case(xs, ys), i) =>
          val interp = LinearInterpolation.withFlatExtrapolation(xs, ys)
          val xMin = xs.head
          val xMax = xs.last
          val xRange = xMax - xMin
          val dx = 0.1 * xRange
          assertEqualsDouble(interp(xMin - dx), ys.head, tol, s"i=$i")
          assertEqualsDouble(interp(xMax + dx), ys.last, tol, s"i=$i")
