package lib

import lib.quantities.Tenor

trait Underlying[T]:

  def currency: Currency

  def tenor: Tenor

  def forward(using Market[T]): Either[Error, Forward[T]]

  final def volatility(using Market[T]): Either[Error, VolatilitySurface[T]] =
    summon[Market[T]].volSurface(currency, tenor)
