package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.Tenor

import java.time.LocalDate

class SwapRate(
    val name: String,
    val tenor: Tenor,
    val spotLag: Int,
    val paymentDelay: Int,
    val fixedPeriod: Tenor,
    val floatingRate: Libor,
    val fixedDayCounter: DayCounter,
    val calendar: Calendar,
    val bdConvention: BusinessDayConvention,
    val stub: StubConvention,
    val direction: Direction,
    val discountCurve: Curve
) extends Underlying:

  def interestPeriod(fixingAt: LocalDate): (LocalDate, LocalDate) =
    val startAt = calendar.addBusinessDays(fixingAt, spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

  def forward(using Market): Either[Error, Forward] =

    for
      discountCurve <- summon[Market].yieldCurve(discountCurve)
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
          val dcf = fixedDayCounter.yearFraction(startAt, endAt)
          dcf * discountCurve.discount(paymentAt)

      val floatingLegValue = floating.foldMap:
        case FloatingCoupon(fixingAt, startAt, endAt, paymentAt) =>
          val dcf = floatingRate.dayCounter.yearFraction(startAt, endAt)
          val discount = discountCurve.discount(paymentAt)
          val floatingForward = liborForward(fixingAt)
          dcf * discount * floatingForward

      floatingLegValue / fixedLegValue
