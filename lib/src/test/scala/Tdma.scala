package lib

class TdmaSuite extends munit.FunSuite:

  test("tdma"):
    val a = Vector(-1.0, -1.0)
    val b = Vector(2.0, 2.0, 2.0)
    val c = Vector(-1.0, -1.0)
    val d = Vector(1.0, 0.0, 1.0)

    val x = Tdma.solve(a.toArray, b.toArray, c.toArray, d.toArray).toVector

    x.zipWithIndex.foreach: (elem, i) =>
      assertEqualsDouble(elem, 1.0, 1e-12, s"i=$i")
