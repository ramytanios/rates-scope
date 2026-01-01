package lib

import lib.quantities.Tenor
import lib.syntax.*

class SwapRate[T: DateLike](
    val tenor: Tenor,
    val spotLag: Int,
    val paymentDelay: Int,
    val fixedPeriod: Tenor,
    val floatingRate: Libor[T],
    val fixedDayCounter: DayCounter,
    val calendar: Calendar[T],
    val bdConvention: dtos.BusinessDayConvention,
    val stub: dtos.StubConvention,
    val direction: dtos.Direction,
    val discountCurve: YieldCurve[T]
) extends SwapLike[T]:

  def currency: dtos.Currency = floatingRate.currency

  def interestPeriod(fixingAt: T): (T, T) =
    val startAt = calendar.addBusinessDays(fixingAt, spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

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

      val floating = Leg.floating(
        from,
        to,
        floatingRate.tenor,
        calendar,
        paymentDelay,
        bdConvention,
        floatingRate.settlementRule,
        stub,
        direction
      )

      val fixedLegValue = fixed.foldMap:
        case FixedCoupon(startAt, endAt, paymentAt) =>
          val dcf = DateLike[T].yearFraction(startAt, endAt)(using fixedDayCounter)
          dcf * discountCurve.discount(paymentAt)

      val floatingLegValue = floating.foldMap:
        case FloatingCoupon(fixingAt, startAt, endAt, paymentAt) =>
          val dcf = DateLike[T].yearFraction(startAt, endAt)(using floatingRate.dayCounter)
          val discount = discountCurve.discount(paymentAt)
          val floatingForward = floatingRate.forward(fixingAt)
          dcf * discount * floatingForward

      floatingLegValue / fixedLegValue
