package lib

object Tdma:

  def solve(
      a: Array[Double], // lower diag
      b: Array[Double], // main diag
      c: Array[Double], // upper diag
      d: Array[Double] // rhs
  ): Array[Double] =

    val n = b.length

    require(a.length == n - 1, "unexpected length of lower diagonal")
    require(c.length == n - 1, "unexpected length of upper diagonal")
    require(d.length == n, "unexpected length of right hand side")

    val x = Array.ofDim[Double](n)

    val b0 = b.toArray
    val d0 = d.toArray

    for i <- 0 to n - 2 do
      val w = a(i) / b0(i)
      b0(i + 1) = b0(i + 1) - w * c(i)
      d0(i + 1) = d0(i + 1) - w * d0(i)

    x(n - 1) = d0(n - 1) / b0(n - 1)
    for i <- n - 2 to 0 by -1 do
      x(i) = (d0(i) - c(i) * x(i + 1)) / b0(i)

    x
