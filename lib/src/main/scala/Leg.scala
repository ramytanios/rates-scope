package lib

import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.Tenor

case class FixedCoupon[T](from: T, to: T, paymentAt: T)

case class FloatingCoupon[T](fixingAt: T, from: T, to: T, paymentAt: T)

object Leg:

  def fixed[T: DateLike](
      from: T,
      to: T,
      period: Tenor,
      calendar: Calendar[T],
      paymentDelay: Int,
      businessDayConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction
  ): Vector[FixedCoupon[T]] =
    Schedule(from, to, period, calendar, businessDayConvention, stub, direction)
      .sliding(2)
      .collect:
        case Vector(startAt, endAt) =>
          val paymentAt = calendar.addBusinessDays(endAt, paymentDelay)
          FixedCoupon(startAt, endAt, paymentAt)
      .toVector

  def floating[T: DateLike](
      from: T,
      to: T,
      period: Tenor,
      calendar: Calendar[T],
      paymentDelay: Int,
      businessDayConvention: BusinessDayConvention,
      indexSettlementRule: SettlementRule[T],
      stub: StubConvention,
      direction: Direction
  ): Vector[FloatingCoupon[T]] =
    Schedule(from, to, period, calendar, businessDayConvention, stub, direction)
      .sliding(2)
      .collect:
        case Vector(startAt, endAt) =>
          val paymentAt = calendar.addBusinessDays(endAt, paymentDelay)
          val fixingAt = indexSettlementRule.fixingDate(startAt)
          FloatingCoupon(fixingAt, startAt, endAt, paymentAt)
      .toVector
