package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.Tenor

class SwapRate[T: TimeLike](
    val name: String,
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
    val discountCurve: Curve
) extends Underlying[T]:

  def interestPeriod(fixingAt: T): (T, T) =
    val startAt = calendar.addBusinessDays(fixingAt, spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

  def forward(using Market[T]): Either[Error, Forward[T]] =

    for
      discountCurve <- summon[Market[T]].yieldCurve(discountCurve)
      liborForward <- floatingRate.forward
    yield t =>
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
          val dcf = TimeLike[T].yearFraction(startAt, endAt)(using fixedDayCounter)
          dcf * discountCurve.discount(paymentAt)

      val floatingLegValue = floating.foldMap:
        case FloatingCoupon(fixingAt, startAt, endAt, paymentAt) =>
          val dcf = TimeLike[T].yearFraction(startAt, endAt)(using floatingRate.dayCounter)
          val discount = discountCurve.discount(paymentAt)
          val floatingForward = liborForward(fixingAt)
          dcf * discount * floatingForward

      floatingLegValue / fixedLegValue
