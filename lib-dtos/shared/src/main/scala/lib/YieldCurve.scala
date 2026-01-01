package lib.dtos

trait YieldCurve[T]

object YieldCurve:

  case class Discounts[T](discounts: Seq[(T, Double)]) extends YieldCurve[T]

  case class ContinuousCompounding[T](rate: Double) extends YieldCurve[T]
