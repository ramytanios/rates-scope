// package entry
//
// import cats.syntax.all.*
// import entry.data.Payoff.*
// import entry.data.Underlying.*
//
// class VanillaPricer[T: lib.DateLike](val market: Market[T]):
//
//   val builder = new Builders(market)
//
//   def price(payoff: data.Payoff[T]): Either[lib.Error, Double] =
//     payoff match
//       case c: data.Payoff.Caplet[T] =>
//         builder.caplet(c).flatMap: caplet =>
//           market.volSurface(caplet.rate.currency, caplet.rate.tenor).flatMap: _volSurface =>
//             _volSurface.surface.toList.traverse: (tenor, skew) =>
//               val (ks, vs) = skew.skew.unzip
//               market.volMarketConventions(caplet.rate.currency, tenor).map: udl =>
//                 val mat =
//                   udl.calendar.addBusinessPeriod(market.t, tenor)(using udl.bdConvention)
//                 mat -> lib.Lazy(lib.VolatilitySkew(ks.toIndexedSeq, vs.toIndexedSeq))
//             .flatMap: data =>
//               val volSurface =
//                 lib.VolatilitySurface(market.t, caplet.rate.forward, data.toIndexedSeq)
//               caplet.price(market.t, volSurface)
//
//       case other => lib.Error.Generic(s"invalid underlying $other").asLeft[Double]
//
//       case Swaption(
//             rate,
//             fixingAt,
//             strike,
//             optionType,
//             annuity,
//             discountCurve
//           ) => throw NotImplementedError()
//
//       case BackwardLookingCaplet(
//             startAt,
//             endAt,
//             rate,
//             paymentCurrency,
//             paymentAt,
//             strike,
//             optionType,
//             discountCurve,
//             stub,
//             direction
//           ) => throw NotImplementedError()
