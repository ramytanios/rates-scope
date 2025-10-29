package lib

import lib.dtos.*
import lib.quantities.Tenor
import lib.syntax.*

class CompoundedSwapRate[T: DateLike](
    val tenor: Tenor,
    val spotLag: Int,
    val paymentDelay: Int,
    val fixedPeriod: Tenor,
    val floatingRate: Libor[T],
    val floatingPeriod: Tenor,
    val fixedDayCounter: DayCounter,
    val calendar: Calendar[T],
    val bdConvention: BusinessDayConvention,
    val stub: StubConvention,
    val direction: Direction,
    val discountCurve: YieldCurve[T]
) extends SwapLike[T]:

  def currency: Currency = floatingRate.currency

  def interestPeriod(fixingAt: T): (T, T) =
    val startAt = calendar.addBusinessDays(fixingAt, spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

  val resetCurve = floatingRate.resetCurve

  def fixedSchedule(from: T, to: T) =
    Leg.fixed(
      from,
      to,
      fixedPeriod,
      calendar,
      paymentDelay,
      bdConvention,
      stub,
      direction
    )

  def forward: Forward[T] =
    t =>
      val (from, to) = interestPeriod(t)

      val fixed = fixedSchedule(from, to)

      // floating leg behaves like a fixed one's schedule
      val floating = Leg.fixed(
        from,
        to,
        floatingPeriod,
        calendar,
        paymentDelay,
        bdConvention,
        stub,
        direction
      )

      val fixedLegValue = fixed.foldMap:
        case FixedCoupon(startAt, endAt, paymentAt) =>
          val dcf = DateLike[T].yearFraction(startAt, endAt)(using fixedDayCounter)
          dcf * discountCurve.discount(paymentAt)

      val floatingLegValue = floating.foldMap:
        case FixedCoupon(startAt, endAt, paymentAt) =>
          discountCurve.discount(paymentAt) * (
            resetCurve.discount(startAt) / resetCurve.discount(endAt) - 1.0
          )

      floatingLegValue / fixedLegValue
