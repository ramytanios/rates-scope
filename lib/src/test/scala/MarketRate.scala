package lib

import lib.quantities.Tenor

enum MarketRate(val currency: Currency, val tenor: Tenor):
  case SOFR extends MarketRate(Currency.USD, Tenor.`1D`)
  case `EURIBOR-3M` extends MarketRate(Currency.EUR, Tenor.`3M`)
  case `EURIBOR-6M` extends MarketRate(Currency.EUR, Tenor.`6M`)

object MarketRate:

  given Conversion[MarketRate, String] = _.toString
