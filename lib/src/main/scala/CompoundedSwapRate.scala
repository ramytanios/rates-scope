package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.Tenor

import java.time.LocalDate

class CompoundedSwapRate(
    val name: String,
    val tenor: Tenor,
    val spotLag: Int,
    val paymentDelay: Int,
    val fixedPeriod: Tenor,
    val floatingRate: Libor,
    val floatingPeriod: Tenor,
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
      resetCurve <- summon[Market].yieldCurve(floatingRate.resetCurve)
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
          val dcf = fixedDayCounter.yearFraction(startAt, endAt)
          dcf * discountCurve.discount(paymentAt)

      val floatingLegValue = floating.foldMap:
        case FixedCoupon(startAt, endAt, paymentAt) =>
          discountCurve.discount(paymentAt) * (
            resetCurve.discount(startAt) / resetCurve.discount(endAt) - 1.0
          )

      floatingLegValue / fixedLegValue
