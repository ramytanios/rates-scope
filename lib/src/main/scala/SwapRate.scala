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
    val startAt = calendar.addBusinessDays(fixingAt, -spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

  def forward(t: LocalDate)(using Market): Either[Error, Double] =

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

    val fixedLegValue = fixed.traverseCollect:
      case FixedCoupon(startAt, endAt, paymentAt) =>
        val dcf = fixedDayCounter.yearFraction(startAt, endAt)
        summon[Market].yieldCurve(discountCurve).map(dcf * _.discount(paymentAt))
    .map(_.sum)

    val floatingLegValue = floating.traverseCollect:
      case FloatingCoupon(fixingAt, startAt, endAt, paymentAt) =>
        val dcf = floatingRate.dayCounter.yearFraction(startAt, endAt)
        for
          discount <- summon[Market].yieldCurve(discountCurve).map(_.discount(paymentAt))
          floatingForward <- floatingRate.forward(fixingAt)
        yield dcf * discount * floatingForward
    .map(_.sum)

    (floatingLegValue, fixedLegValue).tupled.map(_ / _)
