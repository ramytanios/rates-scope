package dtos

sealed trait Payoff[T]

object Payoff:

  case class Caplet[T](
      rate: String,
      fixingAt: T,
      startAt: T,
      endAt: T,
      paymentAt: T,
      paymentCurrency: lib.dtos.Currency,
      strike: Double,
      discountCurve: Curve,
      optionType: lib.dtos.OptionType
  ) extends Payoff[T]

  case class Swaption[T](
      rate: String,
      fixingAt: T,
      strike: Double,
      optionType: lib.dtos.OptionType,
      annuity: lib.dtos.Annuity,
      discountCurve: Curve
  ) extends Payoff[T]

  case class BackwardLookingCaplet[T](
      startAt: T,
      endAt: T,
      rate: String,
      paymentCurrency: lib.dtos.Currency,
      paymentAt: T,
      strike: Double,
      optionType: lib.dtos.OptionType,
      discountCurve: Curve,
      stub: lib.dtos.StubConvention,
      direction: lib.dtos.Direction
  ) extends Payoff[T]
