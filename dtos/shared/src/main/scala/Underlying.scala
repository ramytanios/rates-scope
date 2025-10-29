package dtos

import java.time.Period

sealed trait Underlying[T]

object Underlying:

  case class Libor[T](
      name: String,
      currency: lib.dtos.Currency,
      tenor: Period,
      spotLag: Long,
      dayCounter: DayCounter,
      calendar: Calendar[T],
      resetCurve: Curve,
      bdConvention: lib.dtos.BusinessDayConvention
  ) extends Underlying[T]

  case class SwapRate[T](
      name: String,
      tenor: Period,
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: String,
      floatingRate: String,
      fixedDayCounter: DayCounter,
      calendar: Calendar[T],
      bdConvention: lib.dtos.BusinessDayConvention,
      stub: lib.dtos.StubConvention,
      direction: lib.dtos.Direction,
      discountCurve: Curve
  ) extends Underlying[T]

  case class CompoundedSwapRate[T](
      name: String,
      tenor: Period,
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: String,
      floatingRate: String,
      floatingPeriod: String,
      fixedDayCounter: DayCounter,
      calendar: Calendar[T],
      bdConvention: lib.dtos.BusinessDayConvention,
      stub: lib.dtos.StubConvention,
      direction: lib.dtos.Direction,
      discountCurve: Curve
  ) extends Underlying[T]
