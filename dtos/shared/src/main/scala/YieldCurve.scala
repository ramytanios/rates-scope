package dtos

case class YieldCurve[T](discounts: Seq[(T, Double)])
