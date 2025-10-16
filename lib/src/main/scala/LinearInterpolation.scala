package lib

object LinearInterpolation:

  def withLinearExtrapolation(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): (Double => Double) =

    val n = xs.size

    require(xs.isStrictlyIncreasing, "xs must be strictly increasing")
    require(n == ys.length, "xs and ys size mismatch")
    require(n > 2, "need at least 2 points")

    (x: Double) =>
      if x <= xs.head then ys(0) + (ys(0) - ys(1)) / (xs(0) - xs(1)) * (x - xs(0))
      else if x >= xs.last then
        ys(n - 2) + (ys(n - 2) - ys(n - 1)) / (xs(n - 2) - xs(n - 1)) * (x - xs(n - 2))
      else
        val i = xs.search(x).insertionPoint
        ys(i - 1) + (ys(i - 1) - ys(i)) / (xs(i - 1) - xs(i)) * (x - xs(i - 1))
