package entry

import lib.Currency
import lib.quantities.Tenor

object data:

  case class Curve(ccy: Currency, name: String)

  case class Fixing[T](t: T, value: Double)

  case class YieldCurve[T](discounts: Seq[(T, Double)])

  case class VolatiltySkew(skew: Seq[(Double, Double)])

  case class VolatilitySurface[T](surface: Map[T, VolatiltySkew])

  case class VolatilityCube[T](cube: Map[Tenor, VolatilitySurface[T]])

  trait Underlying[T]
