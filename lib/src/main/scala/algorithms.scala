package lib

import scala.annotation.tailrec
import scala.Ordering.Implicits.*
import lib.algorithms.BinarySearch.InsertionLoc
import lib.algorithms.BinarySearch.Found

object algorithms:

  enum BinarySearch:
    case Found(i: Int)
    case InsertionLoc(i: Int)

  def binarySearchBy[C, A: Ordering](coll: IndexedSeq[C], by: C => A)(elem: A): BinarySearch =
    @tailrec
    def go(from: Int, to: Int): BinarySearch =
      if from == to then InsertionLoc(from)
      else
        val idx = from + (to - from) / 2
        val a = by(coll(idx))
        if elem equiv a then Found(idx)
        else if elem < a then go(from, idx)
        else go(idx + 1, to)

    go(0, coll.size)

  extension [C](coll: IndexedSeq[C])
    def searchBy[A: Ordering](by: C => A)(elem: A): BinarySearch = binarySearchBy(coll, by)(elem)
