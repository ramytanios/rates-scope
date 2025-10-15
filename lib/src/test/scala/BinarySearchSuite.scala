package lib

import lib.algorithms.*
import munit.FunSuite
import lib.algorithms.BinarySearch.InsertionLoc
import lib.algorithms.BinarySearch.Found

class BinarySearchSuite extends FunSuite:

  private case class Foo(num: Double)

  test("binary search"):

    val vs = Vector(1.0, 2.0, 3.0, 4.0).map(Foo.apply)

    assertEquals(vs.searchBy(_.num)(1.0), Found(0))

    assertEquals(vs.searchBy(_.num)(1.5), InsertionLoc(1))

    assertEquals(vs.searchBy(_.num)(-1.0), InsertionLoc(0))

    assertEquals(vs.searchBy(_.num)(5.0), InsertionLoc(4))

    assertEquals(Vector(Foo(0.0)).searchBy(_.num)(0.0), Found(0))
