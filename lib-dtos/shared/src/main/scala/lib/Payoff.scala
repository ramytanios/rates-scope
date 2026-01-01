package lib.dtos

sealed trait Payoff[T]

object Payoff:

  case class Caplet[T](
      rate: String,
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
      rate: String,
      fixingAt: T,
      strike: Double,
      optionType: OptionType,
      annuity: Annuity,
      discountCurve: Curve
  ) extends Payoff[T]

  case class BackwardLookingCaplet[T](
      startAt: T,
      endAt: T,
      rate: String,
      paymentCurrency: Currency,
      paymentAt: T,
      strike: Double,
      optionType: OptionType,
      discountCurve: Curve,
      stub: StubConvention,
      direction: Direction
  ) extends Payoff[T]
