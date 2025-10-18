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

    c(0) = c(0) / b(0)
    d(0) = d(0) / b(0)
    var i = 1
    while i < n - 1 do
      val m = 1.0 / (b(i) - c(i - 1) * a(i))
      c(i) = c(i) * m
      d(i) = (d(i) - d(i - 1) * a(i)) * m
      i += 1

    i = n - 2
    x(n - 1) = d(n - 1)
    while i > -1 do
      x(i) = d(i) - c(i) * x(i + 1)
      i -= 1

    x
