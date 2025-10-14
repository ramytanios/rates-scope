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
        case Vector(startAt, endAt) =>
          val paymentAt = calendar.addBusinessDays(endAt, paymentDelay)
          FixedCoupon(startAt, endAt, paymentAt)
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
        case Vector(startAt, endAt) =>
          val paymentAt = calendar.addBusinessDays(endAt, paymentDelay)
          val fixingAt = indexSettlementRule.fixingDate(startAt)
          FloatingCoupon(fixingAt, startAt, endAt, paymentAt)
      .toVector
