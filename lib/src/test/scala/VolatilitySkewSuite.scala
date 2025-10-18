package lib

class VolatilitySkewSuite extends munit.FunSuite:

  test("should have the right asymptotic behavior"):
    { // right slope is negative
      val ks = Vector(0.0, 0.01, 0.02, 0.04, 0.05)
      val vs = Vector(0.9, 0.6, 0.55, 0.4, 0.35)
      val skew = VolatilitySkew(ks, vs)
      val vR = vs.last
      val kInf = 10
      assertEqualsDouble(skew(kInf), vR * 2.0 / 3.0, 1e-10)
    }

    { // left slope is negative
      val ks = Vector(0.0, 0.01, 0.02, 0.04, 0.05)
      val vs = Vector(0.9, 0.6, 0.55, 0.4, 0.35).reverse
      val skew = VolatilitySkew(ks, vs)
      val vL = vs.head
      val kMInf = -10
      assertEqualsDouble(skew(kMInf), vL * 2.0 / 3.0, 1e-10)
    }
