package lib

import lib.Schedule.Direction
import lib.Schedule.StubConvention
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
    val bdConvention: BusinessDayConvention,
    val stub: StubConvention,
    val direction: Direction,
    val discountCurve: YieldCurve[T]
) extends Underlying[T]:

  def currency: Currency = floatingRate.currency

  def interestPeriod(fixingAt: T): (T, T) =
    val startAt = calendar.addBusinessDays(fixingAt, spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

  def forward: Forward[T] =
    t =>
      val (from, to) = interestPeriod(t)

      val fixed = Leg.fixed(
        from,
        to,
        fixedPeriod,
        calendar,
        paymentDelay,
        bdConvention,
        stub,
        direction
      )

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
