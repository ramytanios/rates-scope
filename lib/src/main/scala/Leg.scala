package lib

import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.Tenor

import java.time.LocalDate

case class FixedCoupon(from: LocalDate, to: LocalDate, paymentAt: LocalDate)

case class FloatingCoupon(fixingAt: LocalDate, from: LocalDate, to: LocalDate, paymentAt: LocalDate)

object Leg:

  def fixed(
      from: LocalDate,
      to: LocalDate,
      period: Tenor,
      calendar: Calendar,
      paymentDelay: Int,
      businessDayConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction
  ): Vector[FixedCoupon] =
    Schedule(from, to, period, calendar, businessDayConvention, stub, direction)
      .sliding(2)
      .collect:
        case Vector(from0, to0) =>
          val paymentAt = calendar.addBusinessDays(to0, paymentDelay)
          FixedCoupon(from0, to0, paymentAt)
      .toVector

  def floating(
      from: LocalDate,
      to: LocalDate,
      period: Tenor,
      calendar: Calendar,
      paymentDelay: Int,
      businessDayConvention: BusinessDayConvention,
      indexSettlementRule: SettlementRule,
      stub: StubConvention,
      direction: Direction
  ): Vector[FloatingCoupon] =
    Schedule(from, to, period, calendar, businessDayConvention, stub, direction)
      .sliding(2)
      .collect:
        case Vector(from0, to0) =>
          val paymentAt = calendar.addBusinessDays(to0, paymentDelay)
          val fixingAt = indexSettlementRule.fixingDate(from0)
          FloatingCoupon(fixingAt, from0, to0, paymentAt)
      .toVector
