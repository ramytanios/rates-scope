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
    def go(low: Int, high: Int): BinarySearch =
      val mid = (low + high) / 2
      val midA = by(coll(mid))
      if elem equiv midA then Found(mid)
      else if low == high && elem < by(coll(low)) then InsertionLoc(low - 1)
      else if low == high && elem > by(coll(low)) then InsertionLoc(low + 1)
      else if elem < midA then go(low, mid - 1)
      else go(mid + 1, high)

    go(0, coll.size - 1)

  extension [C](coll: IndexedSeq[C])
    def searchBy[A: Ordering](by: C => A)(elem: A): BinarySearch = binarySearchBy(coll, by)(elem)
