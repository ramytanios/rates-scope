package lib

import lib.dtos.*
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
  ): IndexedSeq[FixedCoupon[T]] =
    val schedule = Schedule(from, to, period, calendar, businessDayConvention, stub, direction)
    schedule.indices.init.map: i =>
      val startAt = schedule(i)
      val endAt = schedule(i + 1)
      val paymentAt = calendar.addBusinessDays(endAt, paymentDelay)
      FixedCoupon(startAt, endAt, paymentAt)

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
  ): IndexedSeq[FloatingCoupon[T]] =
    val schedule = Schedule(from, to, period, calendar, businessDayConvention, stub, direction)
    schedule.indices.init.map: i =>
      val startAt = schedule(i)
      val endAt = schedule(i + 1)
      val paymentAt = calendar.addBusinessDays(endAt, paymentDelay)
      val fixingAt = indexSettlementRule.fixingDate(startAt)
      FloatingCoupon(fixingAt, startAt, endAt, paymentAt)
