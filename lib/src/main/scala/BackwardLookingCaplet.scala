package lib

import cats.syntax.all.*
import lib.Schedule.Direction
import lib.Schedule.StubConvention
import lib.quantities.*
import lib.syntax.*

import scala.math.pow

class BackwardLookingCaplet[T: DateLike](
    val startAt: T,
    val endAt: T,
    val rate: Libor[T],
    val paymentCurrency: Currency,
    val paymentAt: T,
    val strike: Double,
    val optionType: OptionType,
    val discountCurve: YieldCurve[T],
    val stub: StubConvention,
    val direction: Direction
):
  private val noHolidaysCal = Calendar.all

  def price(t: T, cube: VolatilityCube[T], fixings: Map[T, Double]): Either[Error, Double] =

    val discount = discountCurve.discount(paymentAt)

    def syntheticFutLibor(from: T, to: T): Libor[T] =
      val n = noHolidaysCal.countBusinessDays(from, to)
      val tenor = Tenor.days(n.toInt)
      new Libor[T](
        rate.currency,
        tenor,
        rate.spotLag,
        rate.dayCounter,
        noHolidaysCal,
        rate.resetCurve,
        rate.bdConvention
      )

    def futPrice(futRate: CompoundedRate[T], futStrike: Double): Either[Error, Double] =
      given DayCounter = DayCounter.Act365
      val schedule = futRate.schedule
      futRate.forward(t, fixings).map: forward =>
        val futLibor = syntheticFutLibor(futRate.from, futRate.to)
        val vol = cube(futLibor.tenor)(futRate.firstFixingAt)(futStrike)
        val dt = t.yearFractionTo(futRate.firstFixingAt) +
          schedule.indices.init.toList.foldMap(i =>
            schedule(i).fixingAt.yearFractionTo(schedule(i + 1).fixingAt) *
              pow(schedule(i + 1).startAt.yearFractionTo(futRate.to).toDouble, 2)
          ) / pow(futRate.from.yearFractionTo(futRate.to).toDouble, 2)
        bachelier.price(optionType, forward, futStrike, vol, dt, discount)

    Either
      .raiseWhen(rate.currency != paymentCurrency)(
        Error.Generic("vanilla price does not support quanto")
      )
      .flatMap: _ =>
        Either.raiseWhen((endAt - paymentAt).abs > 7)(
          Error.Generic(s"vanilla pricer does not allow payment convexity")
        )
          .flatMap: _ =>
            if t >= paymentAt then Right(0.0)
            else
              val fullRate = CompoundedRate[T](startAt, endAt, rate, stub, direction)
              val fullDcf = fullRate.dcf
              if t < fullRate.firstFixingAt then
                futPrice(fullRate, strike).map(fullDcf * _)
              else if t >= fullRate.lastFixingAt && t < paymentAt then
                val fullRateValue = (fullRate.fullCompoundingFactor(fixings) - 1) / fullDcf.toDouble
                Right:
                  fullDcf * discount * optionType.intrinsic(fullRateValue, strike)
              else
                val obsIdx = fullRate.findObservationIdx(t)
                val futIdx = obsIdx + 1
                val schedule = fullRate.schedule
                val cf = fullRate.compoundingFactor(schedule(obsIdx).fixingAt, fixings)
                val futRate = new CompoundedRate[T](rate, schedule.slice(futIdx, schedule.length))
                val futDcf = futRate.dcf.toDouble
                val futStrike = ((fullDcf * strike + 1) / cf - 1) / futDcf
                futPrice(futRate, futStrike).map(futDcf * cf * _)
