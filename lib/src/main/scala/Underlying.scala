package lib

import lib.quantities.Tenor

trait Underlying[T]:

  def currency: dtos.Currency

  def tenor: Tenor

  def forward: Forward[T]

  def bdConvention: dtos.BusinessDayConvention

  def calendar: Calendar[T]

  def interestPeriod(fixingAt: T): (T, T)
