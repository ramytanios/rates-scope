package dtos

import java.time.Period

sealed trait Underlying[T]

object Underlying:

  case class Libor[T](
      name: String,
      currency: lib.dtos.Currency,
      tenor: Period,
      spotLag: Int,
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
      fixedPeriod: Period,
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
      fixedPeriod: Period,
      floatingRate: String,
      floatingPeriod: Period,
      fixedDayCounter: DayCounter,
      calendar: Calendar[T],
      bdConvention: lib.dtos.BusinessDayConvention,
      stub: lib.dtos.StubConvention,
      direction: lib.dtos.Direction,
      discountCurve: Curve
  ) extends Underlying[T]
