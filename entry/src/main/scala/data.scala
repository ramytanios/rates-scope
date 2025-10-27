package entry

import lib.*
import lib.quantities.Tenor
import lib.Schedule.StubConvention
import lib.Schedule.Direction

object data:

  case class Rate(name: String)

  case class Curve(ccy: Currency, name: String)

  case class Fixing[T](t: T, value: Double)

  case class YieldCurve[T](discounts: Seq[(T, Double)])

  case class VolatiltySkew(skew: Seq[(Double, Double)])

  case class VolatilitySurface[T](surface: Map[Tenor, VolatiltySkew])

  case class VolatilityCube[T](cube: Map[Tenor, VolatilitySurface[T]])

  sealed trait Underlying[T]

  object Underlying:

    case class Libor[T](
        name: String,
        tenor: Tenor,
        spotLag: Long,
        dayCounter: DayCounter,
        calendar: Calendar[T],
        resetCurve: Curve,
        bdConvention: BusinessDayConvention
    ) extends Underlying[T]

    case class SwapRate[T](
        name: String,
        tenor: Tenor,
        spotLag: Int,
        paymentDelay: Int,
        fixedPeriod: Tenor,
        floatingRate: Rate,
        fixedDayCounter: DayCounter,
        calendar: Calendar[T],
        bdConvention: BusinessDayConvention,
        stub: StubConvention,
        direction: Direction,
        discountCurve: Curve
    ) extends Underlying[T]

    case class CompoundedSwapRate[T](
        name: String,
        tenor: Tenor,
        spotLag: Int,
        paymentDelay: Int,
        fixedPeriod: Tenor,
        floatingRate: Rate,
        floatingPeriod: Tenor,
        fixedDayCounter: DayCounter,
        calendar: Calendar[T],
        bdConvention: BusinessDayConvention,
        stub: StubConvention,
        direction: Direction,
        discountCurve: Curve
    ) extends Underlying[T]

  sealed trait Payoff[T]

  object Payoff:

    case class Caplet[T](
        rate: Rate,
        fixingAt: T,
        startAt: T,
        endAt: T,
        paymentAt: T,
        paymentCurrency: Currency,
        strike: Double,
        discountCurve: Curve,
        optionType: OptionType
    ) extends Payoff[T]

    case class Swaption[T](
        rate: Rate,
        fixingAt: T,
        strike: Double,
        optionType: OptionType,
        annuity: Annuity,
        discountCurve: YieldCurve[T]
    ) extends Payoff[T]

    case class BackwardLookingCaplet[T](
        startAt: T,
        endAt: T,
        rate: Rate,
        paymentCurrency: Currency,
        paymentAt: T,
        strike: Double,
        optionType: OptionType,
        discountCurve: Curve,
        stub: StubConvention,
        direction: Direction
    ) extends Payoff[T]
