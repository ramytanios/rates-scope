package lib

class TdmaSuite extends munit.FunSuite:

  case class Case(
      a: Vector[Double],
      b: Vector[Double],
      c: Vector[Double],
      d: Vector[Double],
      sol: Vector[Double]
  )

  test("tdma"):

    val cases = List(
      Case(
        Vector(-1.0, -1.0),
        Vector(2.0, 2.0, 2.0),
        Vector(-1.0, -1.0),
        Vector(1.0, 0.0, 1.0),
        Vector.fill(3)(1.0)
      ),
      Case(
        Vector(-1.0, -1.0, -1.0),
        Vector(2.0, 2.0, 2.0, 1.0),
        Vector(-1.0, -1.0, -1.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(1.0, 2.0, 3.0, 3.0)
      )
    )

    cases.zipWithIndex.foreach: (c, i) =>
      val sol = Tdma.solve(c.a.toArray, c.b.toArray, c.c.toArray, c.d.toArray).toVector
      (sol zip c.sol).zipWithIndex.foreach:
        case x -> xe -> j =>
          assertEqualsDouble(x, xe, 1e-12, s"i=$i, j=$j")
