package lib

import lib.algorithms.*
import lib.algorithms.BinarySearch.Found
import lib.algorithms.BinarySearch.InsertionLoc

class AlgorithmsSuite extends munit.FunSuite:

  test("binary search"):

    case class Foo(num: Double)

    val vs = Vector(1.0, 2.0, 3.0, 4.0).map(Foo.apply)

    assertEquals(vs.searchBy(_.num)(1.0), Found(0))

    assertEquals(vs.searchBy(_.num)(1.5), InsertionLoc(1))

    assertEquals(vs.searchBy(_.num)(-1.0), InsertionLoc(0))

    assertEquals(vs.searchBy(_.num)(5.0), InsertionLoc(4))

    assertEquals(Vector(Foo(0.0)).searchBy(_.num)(0.0), Found(0))

  test("monotonicity"):

    assertEquals(Vector(1.0, 2.0, 3.0).isStrictlyIncreasing, true)

    assertEquals(Vector(1.0, 1.0, 3.0).isStrictlyIncreasing, false)

    assertEquals(Vector(1.0, -1.0, 3.0).isStrictlyIncreasing, false)
